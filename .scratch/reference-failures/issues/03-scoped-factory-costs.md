# Resolve scoped-factory repeated-specialization costs

Status: needs-triage
Type: task

## Cases and evidence

The [audit map](../map.md) names five exact selector/source pairs. Each reaches
value/cost comparison after complete-output guards and ordinary caller builds.
The caller and unchanged independent reference for each selector are
`test/cases/<selector>/Case.hs` and `test/cases/<selector>/Reference.hs`.

| Selector | Source | Probe | Value | Generated/reference cost |
|---|---|---|---:|---|
| `scoped-factory-case-shadow-int` | `test/cases/scoped-factory-case-shadow/Example.hs` | `repeated-shared-specialization` | 4 | 12 / 9 |
| `scoped-factory-case-shadow-integer` | `test/cases/scoped-factory-case-shadow/Example.hs` | `repeated-shared-specialization` | 8 | 16 / 10 |
| `scoped-factory-guard-shadow-int` | `test/cases/scoped-factory-guard-shadow/Example.hs` | `repeated-shared-specialization` | 4 | 12 / 9 |
| `scoped-factory-guard-shadow-integer` | `test/cases/scoped-factory-guard-shadow/Example.hs` | `repeated-shared-specialization` | 8 | 16 / 10 |
| `manual-scoped-annotations` | `examples/ScopedAnnotations.hs` | `factory/mixed-dictionaries-and-controls` | 36 | 84 / 51 |

The manual fixture also reports case, guard and alpha-renamed repeated probes:
Int returns 4 at cost 12 versus 9; Integer returns 8 at cost 16 versus 10.
All values match at the common boundary sample. These failures concern scoped
factories and specialization reuse; this evidence alone does not establish that
every extra tick comes from source-global sharing.

## Intended behavior and acceptance

Retain scope, alpha-renaming, dictionary partitions and lazy control flow while
matching the unchanged references. Account for factory and observation ticks
before choosing a repair. Do not alter independent references or weaken costs.
Keep complete-output guards and ordinary caller compilation before comparisons;
verify boundary/seeded samples and existing optimized postchecks independently
for every mapped case. Source-global changes require a separate scope decision.

## Reproduce

Prepare the runtime using `patches/README.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=scoped-factory-case-shadow-int
sh scripts/stack-test.sh --test-arguments=scoped-factory-case-shadow-integer
sh scripts/stack-test.sh --test-arguments=scoped-factory-guard-shadow-int
sh scripts/stack-test.sh --test-arguments=scoped-factory-guard-shadow-integer
sh scripts/stack-test.sh --test-arguments=manual-scoped-annotations
```
