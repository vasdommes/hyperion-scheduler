# hyperion-scheduler

A task scheduler built on [hyperion](https://github.com/davidsd/hyperion). Given a
DAG of tasks with memory, CPU and disk-space estimates, it distributes them over
the nodes of a job, moves intermediate files between node-local storage, and
cleans them up when they are no longer needed.

## Building

```sh
stack build
```

## Tests

### Unit tests

Pure tests for `TaskMap` validation, task replacement and placeholders. No
cluster, no filesystem:

```sh
stack test
```

### LinearTransform

An end-to-end scheduler test that computes a chain of linear transformations
`x_{i+1} = A_i . x_i`, one task per product `A_ik x_k`. `A_i` is a cyclic shift
by one position, so the answer is known in advance and is checked at the end.
The point is to generate many small tasks with many dependencies:
`--shift` sets the depth of the task graph and `--dim` its width
(`shift * dim^2` multiplication tasks in total).

#### Locally, without SLURM

```sh
stack exec -- hyperion-scheduler-test local \
  --shift 5 --dim 20 --base-dir /tmp/lt-test
```

Everything runs in one process: workers are spawned as local threads, and a
single node is reported to the scheduler with `--cpus` CPUs (all cores by
default). `--base-dir` defaults to `tmp/hyperion-scheduler-linear-transform-test`
in the current directory and holds the results, the logs, and
`node_local_storage/`, which stands in for a compute node's local scratch so the
file-service and cleanup logic is exercised too.

#### On a SLURM cluster

```sh
export HYPERION_SCHEDULER_TEST_SITE=expanse
stack exec -- hyperion-scheduler-test master \
  --shift 10 --dim 100 --nodes 2 --ntasks-per-node 32 \
  --time 30 --mem 64G
```

Per-site settings (partition, account, scratch directory, hostname strategy)
live in `Hyperion.Scheduler.Test.Config`; add a `Site` constructor to support a
new cluster. Anything passed on the command line overrides the site defaults.

The site comes from `HYPERION_SCHEDULER_TEST_SITE`, not a flag. Workers are
launched by hyperion without our options, and master and workers must agree on
`HyperionStaticConfig`; the environment is inherited through `sbatch`, a flag
would not be. Unset, it defaults to `expanse`.

Results go to `<base-dir>/nodes_<N>_ntasks_<M>/`, so runs with different
allocations can be compared. `--base-dir` defaults to the site's scratch
directory.

#### Output

Both modes write, next to the final vector:

- `task_records.json` — one record per task (start time, runtime, memory, node,
  CPUs, output file sizes)
- `task_stats.json` — the same aggregated per task type
