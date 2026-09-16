# Tasks that add tasks

A task running under `runTasks` can add tasks to the run it belongs to. This
document says how, what the scheduler guarantees, and what it refuses.

## The pieces

| name | module | role |
|---|---|---|
| `SchedulerHandle` | `Hyperion.Scheduler.SchedulerHandle` | What a running task holds: an id and a port to the scheduler instance that runs it, valid until the task returns. |
| `taskClosureWithHandle` | `Hyperion.Scheduler.Task.IsTask` | Like `taskClosure`, but the task also receives its `SchedulerHandle`. Default: ignore the handle. |
| `CustomTaskWithHandle`, `keepOutputs` | `Hyperion.Scheduler.Task.Task` | The `TaskKind` whose computation's `Process` part receives the task's `TaskHandle` (the applicative layer, all that runs while the graph is derived, does not), and the keep flag below. |
| `addTasks`, `addTasksWith` | `Hyperion.Scheduler.Dynamic` | Called inside the task: sends a task map through the handle and returns once the tasks are part of the graph. |
| `FollowUps`, `TaskHandle`, `addFollowUp` | `Hyperion.Scheduler.Task.Task`, `Hyperion.Scheduler.SchedulerHandle`, `Hyperion.Scheduler.Dynamic` | A key declares the keys its task may add; the body names one and the scheduler builds it with the requester's own resolver and configs (below). |
| `taskKeepOutputs` | `Hyperion.Scheduler.Task.IsTask` | Keep the task's node-local outputs until the run ends, for readers added later. |

## Adding tasks

`addTasks handle taskMap` sends a task map to the scheduler and returns once the
tasks are part of the graph. The task does not wait for them to run, so
nothing is held across the addition and the scheduler's deadlock argument is
unchanged. The new tasks may depend on tasks already in the run, finished or
not, or on each other; they get critical-path priorities computed from the
grown graph, progress reporting, records and file bookkeeping like any other
task.

Ordering: the scheduler processes the addition before it processes the
completion of the requesting task (the request is acknowledged before the task
can return), so a task that depends on the requesting task cannot start before
the added tasks exist. A build whose later stages depend on the results of
earlier ones is written as tasks that add the next stage before they finish.

A task that is still waiting on dependencies may also be given *more*
dependencies: an added map may name an existing pending task and the new
dependency set is unioned with the old one. This is how a fan-out whose size is
known only at run time is attached to the task that consumes it: the producer
adds the pieces and extends the consumer, which was waiting on the producer
and so has not started.

A task already in the run may be listed again with dependencies it already
has; the entry is ignored. This matters for maps built with `mkTaskMap` on the
scheduler's node, which prune only the tasks whose outputs that node can see:
a finished task whose node-local output is on another node comes back in the
map, with the dependencies it had before.

The scheduler refuses, with an error that `addTasks` throws in the requesting
task: a dependency that is neither in the run nor among the new tasks; a new
dependency on a task that has already been queued, started or finished; and a
node-local input file that has already been deleted. On the last point: a
node-local file is deleted once every task known to use it has finished, so a
producer whose outputs will be read by tasks added later declares
`taskKeepOutputs` (`keepOutputs` for a `TaskKey`); kept files live until the
end of the run and are deleted then. Files on the shared file system are not
tracked and need nothing.

Two entry points: `addTasks` sends the map (the task type needs `Binary`);
`addTasksWith` sends a `Closure (Process (Map a (Set a)))` that builds the map
on the scheduler's side, for task types that cannot cross the wire
(`WrappedTask`, the type `mkTaskMap` produces) or when building the map should
check something on the scheduler's node, such as skipping tasks whose outputs
already exist. The scheduler itself does not prune added tasks.

## Follow-ups declared by the key

`addTasks` and `addTasksWith` make the requesting task responsible for
building the tasks it adds. For a task built from a `TaskKey` that is the
wrong place: building a task needs a resolver and the configs of every key
in its chain, and a task body receives only its own config and key. Putting
the resolver into the config to smuggle it through (which bfss did for a
while) ties the build code to one resolver type and serialises the resolver
into every task record.

Follow-ups fix this by moving the building to the scheduler. A key declares
the keys of the tasks its running task may add:

```haskell
instance TaskKey RemovalDecisionKey where
  type FollowUps RemovalDecisionKey = '[RemovalDecisionKey, PolySdpAssemblyKey]
  taskKind = CustomTaskWithHandle decisionTask
```

and the body, which receives a `TaskHandle RemovalDecisionKey`, names one of
them:

```haskell
addFollowUp handle (VLeft nextDecisionKey)
addFollowUp handle (VRight (VLeft assemblyKey))
```

The `Variant` is over the declared list, so a body cannot ask for a key its
type did not declare. The scheduler looks up the requesting task by its
handle, and builds the follow-up's task map with `mkTaskMap`, using the
resolver and configs the requesting task was itself built with: when the
task chain makes a task whose key declares follow-ups, it also makes a
builder for them (`taskFollowUps` on `IsTask`, `followUps` on
`WrappedTask`) and stores it beside the task. Tasks whose outputs already
exist on the scheduler's node are pruned as in any `mkTaskMap`, and the map
then joins the run under the same rules as an added map. The body never
sees a resolver and the build code never names one; the resolver type is
fixed only where the run is set up.

A key that may add a task of its own kind (a decision that adds the next
decision) makes the chain instance recursive. GHC resolves it with a
recursive dictionary; the unit test `FollowUpsTest` checks this case, and
the cluster test `followupkeys` runs it.

The old entry points stay: `addTasks` for hand-written `IsTask` types with
a `Binary` instance, `addTasksWith` for anything that must be built by a
closure of the requester's own. New `TaskKey`-based code should use
follow-ups.

### A note on placeholders (not done)

The same idea could apply to placeholders: a placeholder key could declare
its producer (`type Producer CoefficientBatchKey = CoefficientBatchesKey`)
and the chain could substitute the producer's task while building the map,
which would make `replaceTasks` and `placeholdersOfType` unnecessary for
that use. It is not done, because blocks-3d uses placeholders differently
(its `Block3dMonolithKey` produces many block tables, and
`replaceBlock3dPlaceholders` groups placeholders into producers with a
grouping rule of its own), and a scheduler-level rule would have to fit
both uses. bfss needs no scheduler support for its case: an
`{-# OVERLAPPING #-}` `HasTaskChain` instance for the placeholder key that
maps it, with `contramapKey`, onto the producer's task link gives the same
graph with no placeholder ever created (the pattern bfss already uses for
`PriorProjectedBlockKey`). Revisit if a second user wants the declaration.

## Testing

```sh
HYPERION_SCHEDULER_TEST_SITE=expanse \
  stack exec -- hyperion-scheduler-test followups master -p '"shared"' -A '"yun124"'
```

runs a search in rounds twice on an 8-CPU node: each round's decision task
adds the next round, or the final task, inside one `runTasks`; once with the
round files on the shared file system and once under the node-local storage
path, where the blocks keep their state files. Each run checks the final round
and sum against a direct computation, the number of task records against the
number of tasks the search must have created, and, in the node-local case,
that no round file survives the run. The log ends with `Follow-ups test
passed` for each. The string options are Haskell-quoted because the test
program's parser reads them with `auto`.

```sh
HYPERION_SCHEDULER_TEST_SITE=expanse \
  stack exec -- hyperion-scheduler-test followupkeys master -p '"shared"' -A '"yun124"'
```

runs the same search written with `TaskKey` keys, `mkTaskMap` and a resolver
(`Hyperion.Scheduler.Test.FollowUpKeys`): the decision key declares its
follow-ups and its body calls `addFollowUp`; no task holds a resolver. Same
two scenarios and checks; the log ends with `Follow-up keys test passed`.
Passed on Expanse on 2026-09-15 (program `GKHtC`, both scenarios, 41 task
records each, no node-local file left).
The pure part, including the recursive instance, is in the unit test suite
(`stack test hyperion-scheduler:test:hyperion-scheduler-unit-test`).
