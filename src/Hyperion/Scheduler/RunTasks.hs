{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}

module Hyperion.Scheduler.RunTasks
  ( runTasks
  , TaskRecords
  ) where

import Control.Concurrent.STM                       (TVar, atomically, check,
                                                     newTVarIO, readTVar,
                                                     writeTVar)
import Control.Concurrent.Utils                     (Lock, mkExclusiveLock,
                                                     withLock)
import Control.Exception                            (SomeException)
import Control.Distributed.Process                  (ReceivePort, SendPort,
                                                     getSelfPid, newChan,
                                                     receiveChan, sendChan)
import Control.Distributed.Process.Async            (Async)
import Control.Distributed.Process.Async            qualified as Async
import Control.Monad                                (foldM, forM_, forever,
                                                     unless, when)
import Control.Monad.Catch                          (Handler (..), catches,
                                                     finally, try)
import Control.Monad.IO.Class                       (liftIO)
import Control.Monad.Reader                         (lift)
import Control.Monad.Writer                         (Writer, runWriter, tell)
import Data.Binary                                  qualified as Binary
import Data.IORef                                   (IORef, atomicModifyIORef',
                                                     newIORef, readIORef,
                                                     writeIORef)
import Data.List.Extra                              (nubOrd, partition)
import Data.List.NonEmpty                           (NonEmpty (..))
import Data.List.NonEmpty                           qualified as NonEmpty
import Data.Map.Strict                              (Map, (!?))
import Data.Map.Strict                              qualified as Map
import Data.Maybe                                   (catMaybes, fromMaybe,
                                                     isNothing, listToMaybe)
import Data.Set                                     (Set)
import Data.Set                                     qualified as Set
import Data.Time.Clock                              (UTCTime, addUTCTime,
                                                     diffUTCTime,
                                                     getCurrentTime)
import Hyperion                                     (Closure, Job, Process,
                                                     RemoteError, unClosure)
import Hyperion.Log                                 qualified as Log
import Hyperion.Scheduler.ConcurrentQueue           (ConcurrentQueue,
                                                     flushQueue, newQueue,
                                                     readAndFlushQueue,
                                                     readQueue, writeListQueue,
                                                     writeQueue)
import Hyperion.Scheduler.Config                    (Config (..))
import Hyperion.Scheduler.SchedulerHandle           (AddTasksReply (..),
                                                     AddTasksRequest (..),
                                                     HandleId (..),
                                                     SchedulerHandle (..))
import Hyperion.Scheduler.FilePath                  (ClusterFilePath (..),
                                                     VirtualFilePath (..),
                                                     isNodeLocal,
                                                     toClusterFilePath)
import Hyperion.Scheduler.FileService               (FileService, Response (..),
                                                     decrementActiveFileUsages,
                                                     deleteFilesFromAllNodes,
                                                     deleteGlobalFiles,
                                                     fetchFilesToNode,
                                                     incrementActiveFileUsages,
                                                     registerFilesOnNode,
                                                     reserveFilesOnNode,
                                                     withFileService)
import Hyperion.Scheduler.RemoteUtil                (AsyncFailedException (..),
                                                     asyncLinkedLocalJob,
                                                     getJobNodes,
                                                     throwOnAsyncFailed)
import Hyperion.Scheduler.RunTasks.Env              (SchedulerEnv (..),
                                                     isHandleActive,
                                                     newHandleRegistry,
                                                     registerHandle,
                                                     unregisterHandle)
import Hyperion.Scheduler.RunTasks.NodeStatus       (NodeStatus)
import Hyperion.Scheduler.RunTasks.NodeStatus       qualified as NodeStatus
import Hyperion.Scheduler.RunTasks.ProgressMap      (ProgressMap)
import Hyperion.Scheduler.RunTasks.ProgressMap      qualified as ProgressMap
import Hyperion.Scheduler.RunTasks.RemoteRunTask    (RemoteRunTaskResult (..),
                                                     remoteRunTask)
import Hyperion.Scheduler.RunTasks.Shared           (Shared (..), newShared,
                                                     readShared)
import Hyperion.Scheduler.RunTasks.Shared           qualified as Shared
import Hyperion.Scheduler.RunTasks.TaskDistribution (CPUAllocation,
                                                     allocateCpusToTasks,
                                                     distributeTasksToNodesWithScores)
import Hyperion.Scheduler.RunTasks.TaskPriority     (TaskPriority,
                                                     TaskPriorityHelper,
                                                     mkTaskPriorityHelper,
                                                     taskPriority)
import Hyperion.Scheduler.RunTasks.TChangeNotifier  (TChangeNotifier,
                                                     newChangeNotifierIO,
                                                     notifyChangeM,
                                                     runWithRetry)
import Hyperion.Scheduler.StatKey                   (TaskKeyFileInfo (..))
import Hyperion.Scheduler.Stats                     (TaskRecord (..))
import Hyperion.Scheduler.Task                      (IsTask (..), RunStage (..),
                                                     TaskMap, taskInputPaths,
                                                     taskMemoryCapped,
                                                     taskOutputPaths,
                                                     validateTaskMap)
import Hyperion.Scheduler.TaskGraph                 (TaskGraph)
import Hyperion.Scheduler.TaskGraph                 qualified as TaskGraph
import Hyperion.Scheduler.TPrioQueue                (TPrioQueue)
import Hyperion.Scheduler.TPrioQueue                qualified as TPrioQueue
import Hyperion.Scheduler.Types                     (FileSize (..),
                                                     MemorySize (..), Node (..),
                                                     NumCPUs)
import Hyperion.Scheduler.WorkerPool                (TWorker (workerId),
                                                     WorkerPool,
                                                     toReusableWorker,
                                                     withWorkerPool,
                                                     withWorkersFromPool,
                                                     workerCpuIdToString)

-- Each task info has input and output files stored as VirtualFilePath's, i.e. global paths like /expanse/lustre/path/to/task.output
-- In fact, tasks will write their output either to that location (GlobalFilePath), or to a Local storage (NodeLocalFilePath), depending on Scheduler storage policy.
-- Scheduler will store this mapping in a FilePathResolveMap.
-- TODO: use e.g. use Control.Concurrent.Map from https://hackage.haskell.org/package/ctrie-0.2 instead of Shared IO (Map...)?
type FilePathResolveMap = Map VirtualFilePath ClusterFilePath
type FileSizeMap = Map VirtualFilePath FileSize

-- `StallingAt task` means that node is idle
-- but cannot dequeue the task from taskQueue
-- e.g. because local storage is running out of space.
data IsNodeStalling a = NotStalling | StallingAt a
  deriving (Eq, Ord)

-- If all nodes are stalling at the same task (the first task in the taskQueue), then we cannot progress.
-- NB: if some node reports `StallingAt somePreviousTask`, then it is not global stalling yet.
-- allNodesStalling returns False in this case.
allNodesStalling :: Ord a => [IsNodeStalling a] -> Bool
allNodesStalling statuses = case nubOrd statuses of
  [StallingAt _] -> True
  _              -> False

-- | Handle tasks for a single node. When the NodeStatus in
-- 'nodeStatusVar' changes, dequeue as many tasks as will fit given
-- memory and CPU constraints, and run them on the node, updating
-- NodeStatus to account for the new tasks.
--
-- Should we do this atomically?  Well let's imagine that the
-- NodeStatus changes while we are dequeuing tasks. Then we should
-- just dequeue more tasks. We update the NodeStatus when we start the
-- tasks. So, no. We do not need to do things atomically.
runNodeLoop
  :: forall a . IsTask a
  => SchedulerEnv
  -> Node
  -> WorkerPool
  -> Shared IO NodeStatus
  -> TVar (IsNodeStalling a)
  -> TPrioQueue TaskPriority a
  -> TChangeNotifier
  -> Lock
  -> Shared IO FileSizeMap
  -> Shared IO FilePathResolveMap
  -> ConcurrentQueue (SchedulerEvent a)
  -> ConcurrentQueue (TaskRecord a)
  -> SendPort AddTasksRequest
  -> CPUAllocation a
  -> Job ()
runNodeLoop
  env
  node
  workerPool
  nodeStatusVar
  isNodeStallingVar
  taskQueue
  taskQueueNotifier
  taskQueueLock
  fileSizeMapVar
  pathResolveMapVar
  eventQueue
  taskRecordQueue
  addTasksRequestPort
  initialAllocation = do

  forM_ (Map.keys initialAllocation) $ \task -> do
     (response, localFileSizes) <- lift $ reserveLocalTaskFiles task
     case response of
      ReserveSuccess -> pure ()
      _ -> Log.throwError $
        "Failed to reserve local disk space for a task from initial allocation: " <> show
          ( response
          , node
          , taskTag task
          , taskOutputPaths task
          , localFileSizes
          )

  runRemoteTasks initialAllocation
  forever getAndRunTasks

  where
    config = env.config
    fileService = env.fileService

    reserveLocalTaskFiles :: a -> Process (Response, Map VirtualFilePath FileSize)
    reserveLocalTaskFiles t = do
      let localFiles = Set.filter (isNodeLocal config) $ Set.union (taskInputPaths t) (taskOutputPaths t)
      localFileSizes <- liftIO $ Shared.withRead fileSizeMapVar $ flip Map.restrictKeys localFiles
      response <- reserveFilesOnNode fileService localFileSizes node.address
      return (response, localFileSizes)

    -- Try to take the first element of the taskQueue.
    -- In case of failure (no tasks available, or node does not have enough free CPUs, memory or local disk space),
    -- wait and retry when notified by taskQueueNotifier
    -- (i.e. when either other tasks have finished, or other nodes have taken things from the taskQueue).
    -- Finally, return that first element.
    blockUntilNewTask :: Job a
    blockUntilNewTask = runWithRetry taskQueueNotifier $ do
      status <- liftIO $ readShared nodeStatusVar
      let
        freeMem  = node.memory - status.memoryInUse
        freeCpus = node.cpus   - status.cpusInUse
      if freeCpus > 0 then do
        (maybeTask, mNextTaskInQueue) <- dequeueTask freeMem freeCpus

        -- Node is stalling if it is empty and cannot take the next task from taskQueue
        liftIO $ atomically $ writeTVar isNodeStallingVar $
          if status == NodeStatus.empty && isNothing maybeTask then
            case mNextTaskInQueue of
              Just t  -> StallingAt t
              Nothing -> NotStalling
          else
            NotStalling

        return maybeTask
      -- Optimize for the case (freeCpus == 0), avoid extra synchronization in dequeueTask
      else do
        liftIO $ atomically $ writeTVar isNodeStallingVar NotStalling
        return Nothing

    checkChangeActiveUsagesResponse :: Response -> Process ()
    checkChangeActiveUsagesResponse r = case r of
      ChangeActiveUsagesSuccess -> return ()
      _ -> Log.throwError $ "Failed to change active file usages: " ++ show r

    -- Remove a task from the taskQueue if it has memory less than freeMem
    -- and try to reserve space for input/output files on the local storage.
    -- In case of success, returns (read taskQueue, tryPeek taskQueue)
    -- In case of failure (taskQueue is empty or the tasks are too big),
    --   returns (Nothing, tryPeek taskQueue)
    -- NB: we return the second element (the first remaining task in taskQueue) here
    --     instead of calling `tryPeek taskQueue` later
    --     to ensure `atomic` behaviour guarded by taskQueueLock.
    --     This ensures that the correct task is passed to isNodeStallingVar.
    dequeueTask :: MemorySize -> NumCPUs -> Job (Maybe a, Maybe a)
    dequeueTask freeMem freeCpus = lift $ withLock taskQueueLock $ do
      maybeTask <- liftIO $ atomically $ TPrioQueue.tryPeek taskQueue
      case maybeTask of
        Just t -> do
          if (taskMemoryCapped node.memory t <= freeMem && taskMinThreads InProgressRun t <= freeCpus) then do
            (response, localFileSizes) <- reserveLocalTaskFiles t
            case response of
              ReserveSuccess -> do
                -- Mark input files as used, so they won't be deleted until the task finishes.
                -- NB: this should be done now (and not in remoteRunAndUpdateNodeStatus before/after fetching)
                -- to ensure that the Reserve request from the next `dequeueTask` will not try to delete these files.
                _ <- checkChangeActiveUsagesResponse <$> incrementActiveFileUsages fileService (taskInputPaths t) node.address
                (newTask, mNextTaskInQueue) <- liftIO $ atomically $ do
                  liftA2 (,) (TPrioQueue.read taskQueue) (TPrioQueue.tryPeek taskQueue)
                notifyChangeM taskQueueNotifier
                if (newTask /= t) then do
                  let taskInfo' task = (taskTag task, taskOutputPaths task)
                  Log.throwError $
                    "Non-atomic dequeueTask: someone else modified taskQueue without taking taskQueueLock! (peekTask,readTask): "
                    ++ show (taskInfo' t, taskInfo' newTask)
                else
                  return $ (Just newTask, mNextTaskInQueue)
              ReserveLimitExceeded _ _ -> do
                -- TODO: if all CPUs are free, shall we try to run the next task? Makes sense if it would help with cleanup.
                -- TODO: if all CPUs on all nodes are free, we should definitely do something. Either take another task or throw error and exit.
                Log.text $ "WARN: Cannot dequeue task, waiting until more local storage space becomes available: " <> Log.showText
                  -- TODO: we print only one of the files to make logs more compact
                  (response, node.address, taskTag t, listToMaybe $ Map.toAscList localFileSizes)
                pure (Nothing, maybeTask)
              _ -> Log.throwError $ "Failed to reserve space for task files: " ++ show (response, localFileSizes)
          else
            pure (Nothing, maybeTask)
        Nothing -> pure (Nothing, maybeTask)

    -- Grow newTasks by repeatedly dequeueing from the taskQueue until
    -- we have no more free cpus or memory. This operation is not
    -- atomic, so it could happen that the NodeStatus is updated by
    -- other threads while 'getMoreTasks' is running. This could
    -- potentially allow us to get even more tasks, so it is a good
    -- thing.
    getMoreTasks :: NonEmpty a -> Job (NonEmpty a)
    getMoreTasks newTasks = do
      status <- liftIO $ readShared nodeStatusVar
      let
        freeMem  = node.memory - status.memoryInUse - sum (fmap (taskMemoryCapped node.memory) newTasks)
        freeCpus = node.cpus   - status.cpusInUse   - sum (fmap (taskMinThreads InProgressRun) newTasks)
      maybeTask <-
        if freeCpus > 0 then
          fst <$> dequeueTask freeMem freeCpus
        -- Optimize for the case (freeCpus == 0), avoid extra synchronization in dequeueTask
        else
          pure Nothing

      case maybeTask of
        Just t  -> getMoreTasks (NonEmpty.cons t newTasks)
        Nothing -> pure newTasks

    -- Block until there are available CPUs and the first task in the
    -- queue can fit in memory. When those conditions are met, dequeue
    -- all the tasks that can fit on the available resources.
    getNewTasks :: Job (NonEmpty a)
    getNewTasks = do
      newTask <- blockUntilNewTask
      getMoreTasks (NonEmpty.singleton newTask)

    runRemoteTasks :: CPUAllocation a -> Job ()
    runRemoteTasks allocation = do
      let
        allocList = Map.toList allocation
        spawnTask alloc = do
          asyncTaskHandle <- asyncLinkedLocalJob $ remoteRunAndUpdateNodeStatus alloc
          lift $ throwOnAsyncFailed asyncTaskHandle

      -- Add all the tasks to NodeStatus
      liftIO $ Shared.withWrite_ nodeStatusVar $
        \s -> foldr NodeStatus.addTask s allocList
      mapM_ spawnTask allocList

    -- Run a RemoteTask and update NodeStatus when the task is
    -- finished.
    remoteRunAndUpdateNodeStatus :: (a, NumCPUs) -> Job ()
    remoteRunAndUpdateNodeStatus t@(task, numCpus) = withWorkersFromPool workerPool node.address numCpus $ \workers -> do
      -- TODO:
      -- Resolve input/output file paths from taskInfo, using pathResolveMapVar. Put output files to Data.Bimap?
      let localVirtualPaths = Set.filter (isNodeLocal config) $ taskInputPaths task
      pathResolveMap <- liftIO $ Shared.withRead pathResolveMapVar $ flip Map.restrictKeys localVirtualPaths
      let
        resolvePath :: VirtualFilePath -> ClusterFilePath
        resolvePath vp@(VirtualFilePath p) = fromMaybe defaultPath maybeResolvedPath where
          maybeResolvedPath = pathResolveMap !? vp
          -- pathResolveMap should contain all files created in the current Scheduler run.
          -- If some input files (e.g. TTTT blocks) have been computed earlier, they are not in the map.
          -- In that case, we just use the provided path and check that it is a global path.
          defaultPath :: ClusterFilePath
            | isNodeLocal config vp = error $ "Node-local input file not found in FilePathResolveMap! " ++ show vp
            | otherwise      = GlobalFilePath p

        localInputPaths = Set.map resolvePath localVirtualPaths

        checkFetchResponse :: Response -> Process ()
        checkFetchResponse r = case r of
          (FetchSuccess _)   -> return ()
          (FetchError paths) -> Log.throwError $ "Failed to fetch files: " ++ show paths
          _                  -> Log.throwError $ "Illegal Fetch response: " ++ show r

      -- NB: Our current logic implies that underying FilePath does not change when we resolve VirtualFilePath.
      --     Changing paths for the task is not trivial, since we do not supply input/output paths as task arguments.
      --     If we want to change the paths used in the actual computation, we have to change e.g. boundFiles.jsonDir etc.
      --     inputPaths and outputPaths from TaskInfo are not related directly to the actual computation performed by a task
      -- TODO: what about redirecting logs?
      -- NB: We need to fetch only node-local files from other nodes.
      --     We filter files in fileManager.fetchFilesToNode (before sending actual request to NodeLocalFileManager process),
      --     so we don't have to do it here.

      -- TODO: each ReusableWorker writes to its own log file,
      -- but it is currently used only in NodeLocalFileManager and does not print anything.
      -- Shall we redirect all logs to a single file?
      _ <- when (not $ Set.null localInputPaths) $ do
        reusableWorkers <- mapM toReusableWorker workers
        fetchResponse <- lift $ fetchFilesToNode fileService localInputPaths node.address reusableWorkers
        lift $ checkFetchResponse fetchResponse

      -- If numCpus == 0, the task is not in fact running remotely (see CleanupTask and BoundTask).
      -- firstWorker = Nothing in that case.
      -- The names CanRunRemote and remoteRunTask are misleading in this case.
      -- TODO change API to make it clear and explicit?
      let firstWorker = listToMaybe workers

      -- TODO for debug
      selfPid <- lift getSelfPid
      priority <- liftIO $ taskQueue.elemPriority task
      Log.info "remoteRunTask (masterPid,workerId,tag,numCPUs,priority,outputPaths)"
        (selfPid, workerCpuIdToString <$> (.workerId) <$> firstWorker, taskTag task, numCpus, priority, taskOutputPaths task)

      -- The task's handle to this scheduler instance while it runs: its id
      -- is valid until the task returns.
      handleId <- liftIO $ atomicModifyIORef' env.handleCounter $ \n -> (n + 1, MkHandleId n)
      let
        handle = MkSchedulerHandle
          { handleId    = handleId
          , requestPort = addTasksRequestPort
          }
      liftIO $ registerHandle env.handles handleId
      start <- liftIO getCurrentTime
      res <- remoteRunTask firstWorker numCpus handle task
        `finally` liftIO (unregisterHandle env.handles handleId)
      end <- liftIO getCurrentTime
      let
        -- TODO: currently afterReturnRemoteRunTaskResult measures file sizes only for taskOutputs
        pathToFileStatKey = Map.fromList $ map (\info -> (info.path, info.fileStatKey)) $ Set.toList $
          Set.union (taskInputs task) (taskOutputs task)
        -- (VirtualFilePath, FileSize) -> (FileStatKeyHash, NonEmpty FileSize)
        toTaskFileSizeItem (path, size) = (pathToFileStatKey Map.! path, NonEmpty.singleton size)

        outputPathsMap = Map.fromSet (toClusterFilePath config node.address) $ taskOutputPaths task
        onDuplicate key _ _ = error $ "Output file path is used by more than one task: " ++ show key
      -- Add to Sheduler's FilePathResolveMap the files created by this task. This should happen before adding the task to eventQueue.
      liftIO $ Shared.withWrite_ pathResolveMapVar $ Map.unionWithKey onDuplicate outputPathsMap
      liftIO $ Shared.withWrite_ nodeStatusVar $ NodeStatus.removeTask t

      -- Map.union is left-biased and thus will override old file sizes (or estimates)
      liftIO $ Shared.withWrite_ fileSizeMapVar $ Map.union res.remoteTaskFileSizes
      registerRes <- lift $ registerFilesOnNode fileService res.remoteTaskFileSizes node.address
      case registerRes of
        RegisterSuccess -> pure ()
        _               -> Log.throwError $ "Failed to register output files: " ++ show (node.address, registerRes)

      _ <- lift $ checkChangeActiveUsagesResponse <$>
        decrementActiveFileUsages fileService (taskInputPaths task) node.address

      -- TODO: use a node-specific TChangeNotifier here?
      notifyChangeM taskQueueNotifier
      liftIO $ writeQueue taskRecordQueue MkTaskRecord
        { task        = task
        , taskStart   = start
        , taskRuntime = diffUTCTime end start
        , taskMemory  = res.remoteTaskMemory
        , taskNode    = node
        , taskNumCPUs = numCpus
        , taskFileSizes = Map.fromListWith (<>) $ map toTaskFileSizeItem $ Map.toList res.remoteTaskFileSizes
        }
      -- NB: this should be the last operation, since the nodeLoop process is killed
      -- after monitorProgressAndDeps reads the last event from eventQueue!
      liftIO $ writeQueue eventQueue (TaskFinished task)

    getAndRunTasks :: Job ()
    getAndRunTasks = do
      newTasks <- getNewTasks
      status <- liftIO $ readShared nodeStatusVar
      let
        freeCpus = node.cpus - status.cpusInUse
        allocation = allocateCpusToTasks InProgressRun freeCpus (NonEmpty.toList newTasks)
      runRemoteTasks allocation

cleanupLoop :: Config -> FileService -> TChangeNotifier -> CleanupQueue -> Job ()
cleanupLoop config fileService taskQueueNotifier cleanupQueue = lift go where
  go = do
    mPaths <- readAndFlushQueue cleanupQueue
    let
      paths = [p | Just p <- NonEmpty.toList mPaths]
      shouldExit = any isNothing mPaths
      (localPaths, globalPaths) = partition (isNodeLocal config) paths
      -- TODO merge and/or check responses
      checkResponse response = case response of
        FileDeleted -> pure ()
        FileDoesNotExist -> pure ()
        _ -> do Log.err $ "Unexpected response: " <> show (response, localPaths)

    localResponses <- deleteFilesFromAllNodes fileService localPaths
    mapM_ checkResponse localResponses

    globalResponses <- deleteGlobalFiles fileService globalPaths
    mapM_ checkResponse globalResponses

    -- Notify task queue readers that we cleaned up some disk space.
    -- This helps if they couldn't reserve disk space for a task.
    notifyChangeM taskQueueNotifier
    unless shouldExit go


-- | What the node loops and the request handler report to
-- 'monitorProgressAndDeps', which owns every piece of graph state.
data SchedulerEvent a
  = TaskFinished a
    -- | Tasks that a running task asked to add to the run (see
    -- "Hyperion.Scheduler.Dynamic"). The reply is sent once they are part of
    -- the graph, or when they are refused.
  | TasksAdded (Map a (Set a)) (SendPort AddTasksReply)

-- | The graph state of one scheduler instance while it runs.
data MonitorState a = MkMonitorState
  { edges       :: Map a (Set a)
    -- ^ every task of the run so far, with its dependencies
  , taskGraph   :: TaskGraph a
    -- ^ 'TaskGraph.fromEdges' of 'edges'; rebuilt when tasks are added
  , depCounts   :: TaskGraph.DepCounts a
    -- ^ unfinished dependencies of the tasks that have not been queued yet
  , finished    :: Set a
  , cleanup     :: CleanupState
  , progressMap :: ProgressMap
  , lastReport  :: UTCTime
  }

-- | Continually read events from 'eventQueue' and update the graph state,
-- returning when all tasks are completed. Report progress whenever an
-- update comes in, provided it has been at least 'reportInterval' since the
-- last progress report. When all the dependencies of a task have finished,
-- enqueue the task in the 'taskQueue'. Tasks added during the run
-- ('TasksAdded') join the graph here, so that all graph state lives in one
-- thread.
monitorProgressAndDeps
  :: forall a . IsTask a
  => Config
  -> [Node]
  -> TPrioQueue TaskPriority a
  -> TChangeNotifier
  -> Lock
  -> IORef (TaskPriorityHelper a)
  -> Shared IO (Map VirtualFilePath FileSize)
  -> ConcurrentQueue (SchedulerEvent a)
  -> CleanupQueue
  -> Map Node (Shared IO NodeStatus, TVar (IsNodeStalling a))
  -> Map a (Set a)
  -> Job ()
monitorProgressAndDeps config nodes taskQueue taskQueueNotifier taskQueueLock priorityRef fileSizeMapVar eventQueue cleanupQueue nodeStatusMap initialEdges = do
  let
    initialGraph = TaskGraph.fromEdges initialEdges
    initialProgress = ProgressMap.fromTasksTodo (TaskGraph.keys initialGraph)
  Log.info "Building" (catMaybes (Map.keys initialProgress))
  report initialProgress
  start <- liftIO getCurrentTime

  monitorStallingHandle <- lift $ Async.asyncLinked $ Async.task $ do
    waitForGlobalStalling
    _ <- Log.throwError "All nodes are stalling: no tasks are running and the next task cannot be taken from the queue."
    return ()
  lift $ throwOnAsyncFailed monitorStallingHandle

  initialCleanup <- either Log.throwError pure $
    claimFilesOfTasks config (Map.keysSet initialEdges) emptyCleanupState
  finalState <- go MkMonitorState
    { edges       = initialEdges
    , taskGraph   = initialGraph
    , depCounts   = TaskGraph.dependencyCounts initialGraph
    , finished    = Set.empty
    , cleanup     = initialCleanup
    , progressMap = initialProgress
    , lastReport  = start
    }

  _ <- lift $ Async.cancelWait monitorStallingHandle

  finish <- liftIO getCurrentTime
  Log.info "Finished" ( catMaybes (Map.keys finalState.progressMap)
                      , diffUTCTime finish start
                      )
  where
    -- TODO: Order the tags appropriately
    -- TODO: Properly indent the progress bars
    report progressMap = do
      ProgressMap.displayLog progressMap
      nodeStatuses <- liftIO $ traverse (readShared . fst) nodeStatusMap
      Log.text $ "Nodes: \n" <> NodeStatus.display (Map.elems nodeStatuses)

    reportIfAfterInterval lastReportTime progressMap = do
      now <- liftIO getCurrentTime
      if now > addUTCTime config.reportInterval lastReportTime
        then report progressMap >> pure now
        else pure lastReportTime

    go :: MonitorState a -> Job (MonitorState a)
    go st
      | ProgressMap.isFinished st.progressMap = do
          -- Files kept for tasks added during the run are intermediates like
          -- any other node-local file: the run leaves none behind.
          writeListQueue cleanupQueue $ map Just (Set.toList st.cleanup.kept)
          writeQueue cleanupQueue Nothing
          pure st
      | otherwise = do
          event <- liftIO $ readQueue eventQueue
          st' <- case event of
            TaskFinished t                -> taskFinished st t
            TasksAdded newEdges replyPort -> tasksAdded st newEdges replyPort
          go st'

    taskFinished :: MonitorState a -> a -> Job (MonitorState a)
    taskFinished st finishedTask = do
      let
        progressMap' = ProgressMap.update finishedTask st.progressMap
        (depCounts', newTasks) = TaskGraph.decrementReverseDependencies st.taskGraph st.depCounts finishedTask
        (cleanup', pathsToCleanup) = releaseFiles (taskFilesToCleanup config finishedTask) st.cleanup
      withLock taskQueueLock $ mapM_ (TPrioQueue.write taskQueue) newTasks
      writeListQueue cleanupQueue $ map Just pathsToCleanup
      notifyChangeM taskQueueNotifier
      lastReport' <- reportIfAfterInterval st.lastReport progressMap'
      pure st
        { depCounts   = depCounts'
        , finished    = Set.insert finishedTask st.finished
        , cleanup     = cleanup'
        , progressMap = progressMap'
        , lastReport  = lastReport'
        }

    tasksAdded :: MonitorState a -> Map a (Set a) -> SendPort AddTasksReply -> Job (MonitorState a)
    tasksAdded st newEdges replyPort =
      case insertTasks config st newEdges of
        Left err -> do
          Log.warn "Refusing to add tasks" err
          lift $ sendChan replyPort (AddTasksRefused err)
          pure st
        Right (st', runnable, newKeys) -> do
          -- New tasks get critical-path priorities from the grown graph;
          -- tasks already queued keep the priority they were queued with.
          liftIO $ writeIORef priorityRef (mkTaskPriorityHelper nodes st'.taskGraph)
          -- Map.union is left-biased: sizes already measured stay.
          liftIO $ Shared.withWrite_ fileSizeMapVar $ \sizes -> Map.union sizes (fileSizeEstimates newKeys)
          Log.info "Added tasks (tags, new, runnable now)"
            (catMaybes (Set.toList (Set.map taskTag newKeys)), Set.size newKeys, length runnable)
          withLock taskQueueLock $ mapM_ (TPrioQueue.write taskQueue) runnable
          notifyChangeM taskQueueNotifier
          lift $ sendChan replyPort AddTasksDone
          pure st'

    -- Wait until all nodes are stalling.
    waitForGlobalStalling :: Process ()
    waitForGlobalStalling = do
      let isStallingVars = map snd $ Map.elems nodeStatusMap
      liftIO $ atomically $ do
        globalStalling <- allNodesStalling <$> mapM readTVar isStallingVars
        check globalStalling

-- | Add tasks to a running graph. Refused, leaving the state unchanged, if a
-- dependency is neither in the run nor among the new tasks, if a dependency
-- is added to a task that has already been queued or has finished, or if a
-- new task needs a node-local file that has already been deleted. Returns
-- the new state, the tasks that can be queued right away, and the keys that
-- were not in the run before.
insertTasks
  :: IsTask a
  => Config
  -> MonitorState a
  -> Map a (Set a)
  -> Either String (MonitorState a, [a], Set a)
insertTasks config st newEdges0 = do
  let
    existing = Map.keysSet st.edges
    -- A task of the run listed again with dependencies it already has is
    -- ignored: a map built with 'mkTaskMap' on the scheduler's node lists a
    -- finished task whenever its node-local output is on another node.
    redundant t deps = maybe False (Set.isSubsetOf deps) (Map.lookup t st.edges)
    newEdges = Map.filterWithKey (\t deps -> not (redundant t deps)) newEdges0
    allKeys = Set.union existing (Map.keysSet newEdges)
    newKeys = Map.keysSet newEdges `Set.difference` existing
    extended = Map.keysSet newEdges `Set.intersection` existing
    unknownDeps = Set.unions (Map.elems newEdges) `Set.difference` allKeys
    -- A task is pending while it still has a positive dependency count.
    -- Queued and running tasks have count 0 or no entry; finished tasks are
    -- in 'finished'.
    isPending t = not (Set.member t st.finished) && maybe False (> 0) (Map.lookup t st.depCounts)
    notPending = Set.filter (not . isPending) extended
  unless (Set.null unknownDeps) $
    Left $ show (Set.size unknownDeps) <> " dependencies are not tasks of this run" <> tagsOf unknownDeps
  unless (Set.null notPending) $
    Left $ "cannot add dependencies to " <> show (Set.size notPending)
      <> " tasks that have already started or finished" <> tagsOf notPending
  cleanup' <- claimFilesOfTasks config newKeys st.cleanup
  let
    edges' = Map.unionWith Set.union st.edges newEdges
    remaining t = Set.size $ Set.filter (`Set.notMember` st.finished) (edges' Map.! t)
    counts = Map.fromSet remaining (Map.keysSet newEdges)
    (runnableCounts, pendingCounts) = Map.partition (== 0) counts
    -- Pending tasks carry their count; tasks that can run now leave the map,
    -- as they do when a decrement brings them to zero.
    depCounts' = Map.union pendingCounts $ foldr Map.delete st.depCounts (Map.keys runnableCounts)
  pure
    ( st
        { edges       = edges'
        , taskGraph   = TaskGraph.fromEdges edges'
        , depCounts   = depCounts'
        , cleanup     = cleanup'
        , progressMap = ProgressMap.addTasks newKeys st.progressMap
        }
    , Map.keys runnableCounts
    , newKeys
    )
  where
    tagsOf ts = " (tags: " <> show (catMaybes (Set.toList (Set.map taskTag ts))) <> ")"

-- | The declared sizes of the files of the given tasks, used until the
-- actual sizes are known.
fileSizeEstimates :: IsTask a => Set a -> Map VirtualFilePath FileSize
fileSizeEstimates tasks = Map.unions $ map toFileSizeMap $ Set.toList tasks
  where
    toFileSizeMap t = Map.fromList
      [ (info.path, info.fileSize) | info <- Set.toList (Set.union (taskInputs t) (taskOutputs t)) ]

type TaskRecords a = [TaskRecord a]

-- | Run the given tasks on all nodes of the job by initially
-- distributing largest memory tasks to nodes until we can't fit any
-- more. As tasks finish on a node, we start new tasks on that node,
-- given the newly available memory and cpus.
--
-- TODO: Check that no tasks require more memory than an entire node.
runTasks
  :: IsTask a
  => Config
  -> TaskMap a
  -> Job (TaskRecords a)
runTasks config taskMap = do
  _ <- case validateTaskMap taskMap of
    Left err -> Log.throw err
    Right _  -> pure ()
  nodes <- getJobNodes config
  withFileService config nodes $ \fileService ->
    withWorkerPool nodes $ \workerPool ->
      runTasksWith config nodes fileService workerPool taskMap

-- | 'runTasks' once the job's nodes, the file service and the worker pool
-- are in hand.
runTasksWith
  :: IsTask a
  => Config
  -> [Node]
  -> FileService
  -> WorkerPool
  -> TaskMap a
  -> Job (TaskRecords a)
runTasksWith config nodes fileService workerPool taskMap = do
  handles <- liftIO newHandleRegistry
  handleCounter <- liftIO $ newIORef 0
  let
    -- What the node loops and the request handler need of this run.
    env = MkSchedulerEnv
      { config        = config
      , fileService   = fileService
      , handles       = handles
      , handleCounter = handleCounter
      }
    taskGraph = TaskGraph.fromEdges taskMap
  let
    initialPriorityHelper = mkTaskPriorityHelper nodes taskGraph
    getTaskPriority = taskPriority initialPriorityHelper
    -- TODO: check disk space when computing initialAllocs
    -- TODO: seems that it is non-deterministic and often returns empty allocations, how???
    (initialAllocs, moreIndependentTasks) =
      --distributeTasksToNodes nodes (TaskGraph.independentKeys taskGraph)
      distributeTasksToNodesWithScores 0.75 config nodes (TaskGraph.independentKeys taskGraph) getTaskPriority

  -- TODO for debug
  Log.info "initialAllocs (node, numTasks)" $ Map.map Map.size initialAllocs
  -- NB: don't forget to call `notifyChangeM taskQueueNotifier` on any event
  --   that should trigger retrying in `blockUntilNewTask`!
  -- Currently, it's called when:
  -- 1. We add tasks to the queue (`TPrioQueue.write` in `monitorProgressAndDeps`)
  -- 2. We take a task from the queue (`TPrioQueue.read` in `dequeueTask`)
  -- 3. We finish task in `remoteRunAndUpdateNodeStatus`
  --   (i.e. available memory, CPUs and disk space are updated)
  -- TODO: the third case could be handled via separate notifiers for each node.
  taskQueueNotifier <- liftIO newChangeNotifierIO
  -- Tasks added during the run get priorities from the grown graph (see
  -- 'monitorProgressAndDeps'), so the queue reads the current helper when it
  -- queues a task.
  priorityRef <- liftIO $ newIORef initialPriorityHelper
  taskQueue <- TPrioQueue.newIO $ \t -> (`taskPriority` t) <$> readIORef priorityRef
  -- A lock to ensure that `dequeueTask` is atomic.
  taskQueueLock <- liftIO mkExclusiveLock
  -- Will be updated with actual file sizes as they are created.
  fileSizeMapVar <- liftIO $ newShared (fileSizeEstimates (TaskGraph.keys taskGraph))
  pathResolveMapVar <- liftIO $ newShared Map.empty
  eventQueue <- newQueue
  taskRecordQueue <- newQueue
  cleanupQueue <- newQueue
  (addTasksRequestPort, addTasksRequestRecvPort) <- lift newChan
  requestHandler <- asyncLinkedLocalJob $ handleAddTasksRequests env addTasksRequestRecvPort eventQueue
  lift $ throwOnAsyncFailed requestHandler
  -- NB: here we don't need taskQueueNotifier and taskQueueLock,
  -- since no one is accessing taskQueue yet.
  mapM_ (TPrioQueue.write taskQueue) moreIndependentTasks
  nodeLoopMap :: Map Node ((Shared IO NodeStatus, TVar (IsNodeStalling a)), Async ()) <- flip Map.traverseWithKey initialAllocs $
    \node alloc -> do
      nodeStatusVar <- liftIO $ newShared NodeStatus.empty
      isNodeStallingVar <- liftIO $ newTVarIO NotStalling
      -- TODO: move workerPool to nodeStatusVar?
      loopHandle <- asyncLinkedLocalJob $ do
        -- NB: If we don't catch an exception here, then
        -- `withReusableWorkerPool.release` will be called before rethrowing (and logging) it.
        -- `release` will kill other workers, causing other exceptions (DiedDisconnect).
        -- Thus the original exception will be lost.
        -- To prevent that, we catch, log and rethrow it.
        -- NB: `cancelWait` send `kill`signal to `runNodeLoop`,
        -- thus catching @SomeException will lead to logging unnecessary error message:
        --   ERROR: killed-by=pid://exp-9-55.expanse.sdsc.edu:41403:0:26,reason=cancel
        -- Thus we choose catch only certain popular exception types.
        -- (Exception type for `kill` is not exposed, so we cannot ignore `kill` and catch everything else.)
        -- For example, AsyncFailedException is thrown when a remote task throws exception.
        -- TODO: what other exceptions should we catch?
        -- TODO: implement graceful exit for runNodeLoop.
        runNodeLoop
          env
          node
          workerPool
          nodeStatusVar
          isNodeStallingVar
          taskQueue
          taskQueueNotifier
          taskQueueLock
          fileSizeMapVar
          pathResolveMapVar
          eventQueue
          taskRecordQueue
          addTasksRequestPort
          alloc
          `catches`
            [ Handler (Log.throw @_ @IOError)
            , Handler (Log.throw  @_ @AsyncFailedException)
            , Handler (Log.throw  @_ @RemoteError)
            ]
      lift $ throwOnAsyncFailed loopHandle
      pure ((nodeStatusVar, isNodeStallingVar), loopHandle)
  let
    nodeLoopHandleMap = fmap snd nodeLoopMap
    nodeStatusMap  = fmap fst nodeLoopMap

  cleanupLoopHandle <- asyncLinkedLocalJob $ cleanupLoop config fileService taskQueueNotifier cleanupQueue
  lift $ throwOnAsyncFailed cleanupLoopHandle

  monitorProgressAndDeps
    config
    nodes
    taskQueue
    taskQueueNotifier
    taskQueueLock
    priorityRef
    fileSizeMapVar
    eventQueue
    cleanupQueue
    nodeStatusMap
    taskMap
  -- NB: we use cancelWait instead of e.g. cancelKill to ensure that nodeLoop task will finish with AsyncCancelled.
  -- Finishing with AsyncFailed or AsyncLinkFailed will trigger throwOnAsyncFailed.
  mapM_ (lift . Async.cancelWait) nodeLoopHandleMap
  _ <- lift $ Async.wait cleanupLoopHandle
  _ <- lift $ Async.cancelWait requestHandler
  flushQueue taskRecordQueue

-- | Answer the requests that running tasks send through their
-- 'SchedulerHandle' (see
-- "Hyperion.Scheduler.Dynamic"): tasks to add to this run. The payload is a
-- closure that builds the task map on this side, with this run's task type,
-- so the scheduler need not know how the requesting task represents tasks;
-- the monitor then decides whether they can join the graph and replies.
-- Refusals are reported to the requesting task, not thrown here.
handleAddTasksRequests
  :: forall a . IsTask a
  => SchedulerEnv
  -> ReceivePort AddTasksRequest
  -> ConcurrentQueue (SchedulerEvent a)
  -> Job ()
handleAddTasksRequests env recvPort eventQueue = forever $ do
  request <- lift $ receiveChan recvPort
  let
    refuse msg = do
      Log.warn "Refusing request to add tasks (handle, reason)" (request.handleId, msg)
      lift $ sendChan request.replyPort (AddTasksRefused msg)
  active <- liftIO $ isHandleActive env.handles request.handleId
  if not active
    then refuse $ "unknown or expired handle " <> show request.handleId
    else do
      built <- try $ do
        buildTasks <- lift $ unClosure (Binary.decode request.payload :: Closure (Process (Map a (Set a))))
        taskMap <- lift buildTasks
        pure $! Map.size taskMap `seq` taskMap
      case built of
        Left (e :: SomeException) -> refuse $ "could not build the tasks to add: " <> show e
        Right taskMap -> do
          Log.info "Tasks to add requested (handle, count)" (request.handleId, Map.size taskMap)
          liftIO $ writeQueue eventQueue (TasksAdded taskMap request.replyPort)

-- Files that can be removed (if there are no other dependencies)
-- when the task finished
-- NB: if an output file is never used by any other task, we should delete it too.
-- That's why we take both input and output paths.
-- TODO: remove also all global files with FileTreatment = RemoveFile?
-- TODO: make isNodeLocal configurable?
taskFilesToCleanup
  :: (IsTask a)
  => Config
  -> a
  -> Set VirtualFilePath
taskFilesToCleanup config task = Set.filter (isNodeLocal config) $ taskInputPaths task <> taskOutputPaths task

-- | Bookkeeping of node-local files: how many unfinished tasks use each file
-- (deleted when the count reaches zero), the files kept until the end of the
-- run ('taskKeepOutputs'), and the files already deleted, so that a task
-- added later ("Hyperion.Scheduler.Dynamic") cannot ask for one of them.
data CleanupState = MkCleanupState
  { counts  :: Map VirtualFilePath Int
  , kept    :: Set VirtualFilePath
  , cleaned :: Set VirtualFilePath
  }

emptyCleanupState :: CleanupState
emptyCleanupState = MkCleanupState { counts = Map.empty, kept = Set.empty, cleaned = Set.empty }

-- Nothing means "no more cleanups expected, exit"
type CleanupQueue = ConcurrentQueue (Maybe VirtualFilePath)

-- | Account for the node-local files of tasks joining the run. Refused if one
-- of them has already been deleted: a task added late cannot read a file
-- whose last known user has finished, unless its producer kept it.
claimFilesOfTasks
  :: IsTask a
  => Config
  -> Set a
  -> CleanupState
  -> Either String CleanupState
claimFilesOfTasks config tasks state0 = foldM claimTask state0 (Set.toList tasks)
  where
    claimTask st t = foldM (claim (keptOutputs t)) st (Set.toList (taskFilesToCleanup config t))
    keptOutputs t
      | taskKeepOutputs t = Set.filter (isNodeLocal config) (taskOutputPaths t)
      | otherwise         = Set.empty
    claim keep st path
      | Set.member path st.kept    = Right st
      | Set.member path keep       = Right st { kept = Set.insert path st.kept, counts = Map.delete path st.counts }
      | Set.member path st.cleaned = Left $ "node-local file already deleted: " <> show path
      | otherwise                  = Right st { counts = Map.insertWith (+) path 1 st.counts }

-- | A task finished: release its node-local files. Returns the files whose
-- last user this was, to be deleted.
releaseFiles :: Set VirtualFilePath -> CleanupState -> (CleanupState, [VirtualFilePath])
releaseFiles paths st =
  ( st { counts = counts', cleaned = Set.union st.cleaned (Set.fromList toClean) }
  , toClean
  )
  where
    (counts', toClean) = runWriter $
      decrementCleanupCounts (paths `Set.difference` st.kept) st.counts

type CleanupDependenciesMap = Map VirtualFilePath Int

decrementCleanupCount
  :: VirtualFilePath
  -> CleanupDependenciesMap
  -> Writer [VirtualFilePath] CleanupDependenciesMap
decrementCleanupCount path depMap = Map.alterF go path depMap where
  go (Just i)
    | i > 1     = pure (Just (i-1))
    | otherwise = tell [path] >> pure Nothing
  go Nothing = error "decrementCleanupCount: path not present in map"

decrementCleanupCounts
  :: Set VirtualFilePath
  -> CleanupDependenciesMap
  -> Writer [VirtualFilePath] CleanupDependenciesMap
decrementCleanupCounts paths depMap =
  foldM (\m p -> decrementCleanupCount p m) depMap (Set.toList paths)
