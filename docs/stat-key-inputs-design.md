# Stat keys and input summaries

How the scheduler estimates a task's memory, runtime and output file sizes,
and how recorded statistics correct those estimates.

In short:

- **Stat key = identity.** Statistics are recorded and looked up under it. It
  is a reduced projection of the task alone: its key and the relevant parts of
  its config.
- **Input summary = features.** A reduced view of the task's input files,
  usually their sizes. The stat key type chooses the summary type.
- **Estimate = model(stat key, input summary).**
- **Statistics = observations of (input summary, measured usage) per stat
  key.** They replace the model when the summary matches exactly, and correct
  it when the summary is close.

The code is in `Hyperion.Scheduler.StatKey` (classes, summaries and file
types), `Hyperion.Scheduler.Task.IsTask` (`TaskShape` and `TaskEstimation`),
`Hyperion.Scheduler.Task.Task` (`taskShapeOf`),
`Hyperion.Scheduler.Task.EstimatedTask` (`applyStats`),
`Hyperion.Scheduler.Task.EstimatedTaskMap` (input sizes),
`Hyperion.Scheduler.Stats` (statistics and accuracy) and
`Hyperion.Scheduler.RunTasks.Report` (what `runTasks` logs).

## 1. Why the stat key is not enough

If the stat key were also the only input to the estimates, it would have two
jobs:

1. **Identity.** Statistics are grouped by it and looked up by exact match.
2. **The only input to estimates.** `memoryEstimate`, `runtimeEstimate` and
   `fileSizeEstimate` would see nothing else.

This works for a task whose cost depends only on its own parameters, e.g. a
`blocks_3d` run. It fails for a task whose cost depends on its **inputs**.
Input sizes are properties of other nodes in the task graph. Job 2 forces them
into the key, and job 1 then makes them part of the identity. Downstream keys
held them in two ways, and both are wrong:

| Input information in the key | Example                                                                      | What goes wrong                                                                                                                         |
|------------------------------|------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| A number from another model  | `total_input_size` in `PartChunkStatKey`, `PartStatKey`, `OPEMatrixStatKey` | The identity changes when another task's size model changes, so recorded statistics are lost. The key and the file size need two definitions that must agree. |
| The inputs' stat keys        | `block3dStatKeys` in `CompositeBlockStatKey`                                 | Keys contain keys, so the key copies part of the task graph. PartChunk input keys measured up to 57 KB of JSON.                          |

The input dependence is real. In a TSigEps nmax18 run, PartChunk memory
**within one sector** ranged from 47 MB to 1.7 GB. The large chunks read one
large block table. The chunk's own parameters cannot predict this; its input
sizes can.

The task graph already has the information: its edges, the producers' size
estimates, and the sizes of files already on disk.

## 2. Design

### 2.1 Types

```haskell
-- | What every task knows about one of its input files, whatever the key type.
data InputFile = MkInputFile
  { fileStatKey :: Maybe FileStatKey
  , size        :: FileSize
  }

-- | A summary built one input file at a time, without the dependency key types.
class Monoid s => FromInputFiles s where
  fromInputFile :: InputFile -> s

-- Stock summaries:
instance FromInputFiles ()                  -- the estimates ignore the inputs
newtype MaxInputFileSize    = ...           -- the largest size
newtype TotalInputFileSize  = ...           -- the sum of sizes
newtype InputFileSizes      = ...           -- sorted sizes (a multiset)
newtype KeyedInputFileSizes = ...           -- sorted [(Maybe FileStatKey, FileSize)]
-- and pairs of summaries.

class (Ord s, ToJSON s, FromJSON s) => IsSummary s where
  summaryDistance :: s -> s -> Maybe Double   -- close: 0 to 1, see 2.5

class (Typeable a, ToJSON a, FromJSON a, IsSummary (InputSummary a)) => IsStatKey a where
  type InputSummary a
  type InputSummary a = ()
  memoryEstimate  :: a -> InputSummary a -> MemorySize
  runtimeEstimate :: a -> InputSummary a -> NumCPUs -> NominalDiffTime
  runtimeEstimate k s = defaultRuntimeEstimate (memoryEstimate k s)
  closeInputSummaries :: a -> InputSummary a -> InputSummary a -> Bool

class (Typeable a, ToJSON a, IsSummary (ProducerSummary a)) => IsFileStatKey a where
  -- | What the size estimate needs to know about the input files of the task
  -- that produces this file.
  type ProducerSummary a
  type ProducerSummary a = ()
  fileSizeEstimate :: a -> ProducerSummary a -> FileSize
  closeProducerSummaries :: a -> ProducerSummary a -> ProducerSummary a -> Bool
```

At the `TaskKey` level, summaries are built from the typed dependency keys and
their files. Both methods have defaults through `FromInputFiles`:

```haskell
-- | A task's dependencies with their files' stat keys and sizes.
type DepInputs k = [(Variant (DepKeys k), InputFile)]

class ... => TaskKey k where
  toStatKey :: TaskConfig k -> k -> Maybe (StatKeyOf k)

  toInputSummary :: TaskConfig k -> k -> DepInputs k -> InputSummary (StatKeyOf k)
  default toInputSummary :: FromInputFiles (InputSummary (StatKeyOf k)) => ...
  toInputSummary _ _ = summarizeInputFiles

  toProducerSummary
    :: TaskConfig k -> k -> DepInputs k -> ProducerSummary (FileStatKeyOf (OutKey k))
  -- the same default
```

What a task writes:

| Case                                         | Example                            | Code                                                    |
|----------------------------------------------|------------------------------------|---------------------------------------------------------|
| Estimates ignore inputs                      | Block3d                            | nothing                                                 |
| Stock summary                                | PartChunk memory: largest input    | `type InputSummary PartChunkStatKey = MaxInputFileSize` |
| Custom summary needing typed dependency keys | SDPB run: objective vs constraints | override `toInputSummary` in the `TaskKey` instance     |

The logic of a stock summary lives once, in its `FromInputFiles` instance. Only
custom summaries depend on the dependency key types.

Rules:

- A field belongs in the stat key only if it can be computed from this task
  without looking at other nodes: no input sizes, no input keys.
- A summary sees only the task's **direct** dependencies and their sizes, never
  their own inputs. A node looks at its neighbours, not at the graph. This keeps
  keys and summaries from copying the graph.
- `toInputSummary` and `toProducerSummary` are pure functions of the
  dependency keys and sizes. The scheduler computes them twice: when planning,
  with estimated sizes, and after the task ran, with measured sizes for the
  record.
- `fileStatKeyOf` is structural, a property of the output key alone. One
  definition serves both a produced file and a file already on disk.

### 2.2 Shape and estimation

What a task knows splits by when it is known:

- **`TaskShape`, fixed per task.** Its input and output files (`TaskFile`:
  path and file stat key, no size), its stat key, its models of encoded
  summaries (`Model`), and `estimate :: InputInfos -> TaskEstimation`. The
  traversal of the dependencies and the projections of the stat keys are the
  expensive part. `taskShape` is the only `IsTask` method that gives them.
  `WrappedTask` caches its result for the work before estimation, on the maps
  `mkTaskMap` builds, and `EstimatedTask` caches it for the run, whatever the
  task type.
- **`TaskEstimation`, per set of input sizes.** The files with sizes
  (`SizedTaskFile`), the estimates, and the encoded input and producer
  summaries. `estimate`
  reuses the shape: it only folds the sizes into input summaries and
  evaluates the model.

`estimate` runs twice per task: in `mkEstimatedTaskMap` with the planned
sizes (2.3), and after the task ran with the measured ones, for the record
(2.4). Only an `EstimatedTask` holds an estimation, so estimates cannot be read
before `mkEstimatedTaskMap`: `taskInputs`, `taskOutputs` and
`taskResourceEstimates` take an `EstimatedTask`, while `taskInputPaths`,
`taskOutputPaths` and `taskStatKey` read the shape of any `IsTask`.

### 2.3 Where input sizes come from

`mkEstimatedTaskMap` first validates the map (`validateTaskMap`). Then each
input file gets its size from one of two sources:

1. **Produced in this map:** its producer's output size estimate, after
   statistics (2.5), computed from the producer's own input summary. The map
   is valid, so the producer is one of the task's dependencies.
2. **Otherwise, on disk:** its size on disk, from
   `MonadTaskFiles.taskFileSize`. A file that is not there is an error: the
   task could not run. Maps are built under `runTaskFiles config`, which sees
   node-local paths as absent: the file service does not know such files from
   before the run, so their producers stay in the map. `runTasks` rejects a
   map with a node-local input that no task produces.

So every input of every task has a size, and `estimate` takes a total
`InputInfos`. Tasks are estimated in one pass in dependency order, so sizes
flow along the graph from the leaves. A consumer must see its producers'
*corrected* sizes, which is why statistics are applied in the same pass,
producers first.

`runTasks` accepts only an `EstimatedTaskMap`, whose constructor is hidden, so
a map cannot skip this step.

### 2.4 What is recorded

Summaries are stored encoded, like stat keys (`EncodedSummary`). A
`TaskRecord` has `taskInputSummary` and `taskProducerSummary`. Both are
computed from the sizes the input files had when the task ran, so observations
use real sizes, not planning estimates.

Statistics group observations by summary within each key:

```haskell
newtype TaskStats = MkTaskStats (Map StatKey (Map EncodedSummary TaskResourceMap))
newtype FileStats = MkFileStats (Map FileStatKey (Map EncodedSummary (Trials FileSize)))
```

A stock summary like `MaxInputFileSize` is one number, so a key has few
entries. A richer summary costs more storage, paid only by the key that chose
it.

### 2.5 How statistics are used

For a task with stat key `k` and input summary `s`, `applyStats`
takes, for each figure:

1. **Exact match:** statistics hold `(k, s)`. Use the measurement
   (`MeasuredFromStats`).
2. **Close summaries:** statistics hold `k` with summaries `s₁…sₙ` close to
   `s`. For each observation, take the ratio `rⱼ = measuredⱼ / model(k, sⱼ)`.
   The estimate is `model(k, s) × correction` (`CorrectedByStats`), where the
   correction is `max rⱼ` for memory and file size (never under-predict), and
   the mean ratio at each `NumCPUs` for runtime, then fitted over CPU counts as
   measured runtimes are.
3. **Otherwise** the model alone (`EstimatedByTask`).

Closeness: `summaryDistance` returns `Nothing` when two summaries cannot be
compared. By default a summary is categorical, comparable only to itself, so
rule 2 never applies to it. The size summaries count doublings (the log2 of
their ratio), and a pair is compared when both parts can be. By default two
summaries are close at a distance between 0 and 1 (`nearSummaries`), i.e.
sizes within a factor of 2. `closeInputSummaries` and `closeProducerSummaries`
use that; a key overrides them when its model's ratio holds over a different
range.

Properties:

- **A key whose estimates ignore the inputs** has the summary `()`, so rule 1
  always applies: an exact lookup by stat key.
- **Model changes don't invalidate statistics.** Ratios are computed when
  planning, with the *current* model, from the stored observations. A changed
  block size model changes the input sizes and the ratios together.
- **Statistics from other inputs still help**, e.g. a scan point with slightly
  different blocks.

Rule 2 evaluates the model at stored summaries, which needs the typed stat key
and summary. `WrappedTask` hides the type, so `TaskShape` holds a `Model`: a
decoder, the model and the closeness test of a hidden summary type, built from
the typed key in `taskShapeOf`. `prepareStats` decodes the stored summaries of
each stat key, and evaluates the model at them, once for all the tasks with
that key. So tasks with equal stat keys must have equal models. A stored
summary that does not decode, e.g. after the summary type changed, is skipped.

### 2.6 Reports

- Before a run, `statsCoverage` counts tasks using rule 1, rule 2, and tasks
  with a stat key. `runTasks` warns when nothing matched.
- `accuracyBy` scores records against the estimate used for scheduling or
  against the model, grouped by stat key or by tag.
- `modelAccuracyOf @k` and `modelFileSizeAccuracyOf @k` evaluate the current
  model of key type `k` at each record's stat key and summaries. A changed model
  can then be judged offline against recorded runs.
- `readTaskStats` warns when most stat keys hold a single observation. It
  counts per stat key, not per (key, summary): the stat key is what should
  group many observations.

## 3. Downstream keys

How the stress-tensors-3d keys use this:

| Key                     | Stat key                                                       | Summaries                                                                                              |
|-------------------------|----------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `PartChunkStatKey`      | ty, label, chunk index, precision, bound; no input size         | `InputSummary = MaxInputFileSize`: memory ≈ baseline + c·max input. `ProducerSummary = TotalInputFileSize`. |
| `PartStatKey`           | ty, label, precision, bound; no input size                      | Both `TotalInputFileSize`.                                                                              |
| `OPEMatrixStatKey`      | no input size                                                   | `InputSummary = TotalInputFileSize`.                                                                    |
| `CompositeBlockStatKey` | no embedded input keys                                          | Both `MaxInputFileSize`.                                                                                |
| Block3d keys            | own parameters                                                  | `()`.                                                                                                   |

A chunk with no inputs gets a baseline in its model, not a special case in the
key.

## 4. Decisions

- **The default summary is `()`, not `Void`.** A key whose estimates ignore the
  inputs has exactly one summary. `Void` has none, so its estimates could not be
  called. `()` also puts all observations of a key into one group.
- **`FromInputFiles` is a class on the summary type alone**, not on (summary,
  dependency type). For PartChunk, `DepKeys` is the abstract `SDPFetchKeys b p`,
  and `All (C s) (SDPFetchKeys b p)` cannot be solved from a universal instance
  without an induction proof. A generic `Variant` instance would overlap with
  the universal ones.
- **Correction by the largest ratio** for memory and file size. It is
  conservative, but one outlier raises every estimate of the key. Rejected for
  now: a high quantile, or the maximum over the nearest observations. With no
  close observation, the model is used alone.
- **Task files are regular files, never directories.** `MonadTaskFiles` reads a
  file's existence and size with one `stat`, memoized while building a task
  map, so planning costs about one `stat` per input file. A directory is an
  error.
- **No compatibility with old statistics or records.** The formats will change
  again; old stats files are not read.

## 5. Open questions

- **Exact size of SDPB outputs.** `ChunkShape` in sdpb-haskell, plus the input
  degree, would let PartChunk and Part compute their output size exactly,
  instead of from input sizes.
