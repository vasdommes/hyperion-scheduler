# Tasks that add tasks

A task running under `runTasks` can add tasks to the run it belongs to. This
document says how, what the scheduler guarantees, and what it refuses.

## The pieces

| name | module | role |
|---|---|---|
| `SchedulerHandle` | `Hyperion.Scheduler.SchedulerHandle` | What a running task holds: an id and a port to the scheduler instance that runs it, valid until the task returns. |
| `taskClosureWithHandle` | `Hyperion.Scheduler.Task.IsTask` | Like `taskClosure`, but the task also receives its `SchedulerHandle`. Default: ignore the handle. |
| `CustomTaskWithHandle`, `keepOutputs` | `Hyperion.Scheduler.Task.Task` | The `TaskKind` whose computation receives `Maybe SchedulerHandle` (`Nothing` while the graph is derived), and the keep flag below. |
| `addTasks`, `addTasksWith` | `Hyperion.Scheduler.Dynamic` | Called inside the task: sends a task map through the handle and returns once the tasks are part of the graph. |
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
