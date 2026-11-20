# Separate passing reference regressions from known failures

## Goal

Make the passing reference suite a strict regression baseline. Keep existing
failures easy to inspect and reproduce as deferred issues.

The user approved this policy:

- The default run executes passing reference cases and infrastructure checks.
- A separate run executes every known-failure case with its original assertions.
  It exits nonzero while any case fails.
- A new failure never moves automatically into the known-failure inventory.
- An improving case is reported as passing and moves back through an explicit,
  reviewed inventory change.

## Classification

Identify a reference case by its selector and source file. Classify each source
variant independently, including variants sharing a selector or reference.
Derive the initial classification from fresh executions of the shared reference
lifecycle at a pinned commit.

Keep expected-negative diagnostic and checker-sensitivity tests in the passing
infrastructure suite. Missing tools, runtime setup failures, process crashes and
incomplete executions block classification; they are not deferred semantic
failures.

The fixture inventory remains authoritative. New cases enter the passing suite
by default. Reject unknown, duplicate or stale known-failure entries.

## Execution and inspection

Preserve the common translation, complete-output guards, ordinary caller builds,
independent references, samples and existing optimized postchecks. Every executed
case receives the same checks regardless of its classification.

Expose commands to list both partitions, run either partition and audit all
reference cases. Existing explicit selectors retain access to all their source
variants. Continue after individual case failures so one result cannot hide
another. Preserve interruption and fatal setup errors.

Report each case's selector, source file and result. Retain full failure logs and
artifact locations. Summaries distinguish passing regressions, known failures,
new regressions and recovered cases.

Each deferred case links to a local issue under
`.scratch/reference-failures/issues/`. An issue records its source/reference
paths, observed failure, intended behavior and reproduction command. It may
group cases with the same evidenced cause; it must name every grouped case.

## Completion criteria

1. Public CLI and inventory tests establish selection, independent variants,
   continuation, exit status and strict inventory validation.
2. A fresh audit accounts for every discovered reference source variant.
3. The default passing suite succeeds with the pinned `hs-pbt` setup.
4. The known-failure run visits every deferred case, preserves its assertions,
   reports every result and exits nonzero while failures remain.
5. Listings, local issues and human-facing documentation match the actual CLI.
6. Aggregate validation and independent Standards/Spec review pass.

## Scope

This work organizes tests; it does not repair their underlying translator or
checker defects. Keep reference values, cost assertions, failure behavior and
pre-caller guards unchanged. Success of the passing partition is not success of
the complete reference inventory.
