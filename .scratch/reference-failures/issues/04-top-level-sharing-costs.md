# Resolve top-level sharing costs across exported calls

Status: needs-triage
Type: task

## Cases and evidence

Three cases at the [audit pin](../map.md) pass complete-output guards and ordinary
caller compilation, then fail value/cost comparison. For each selector below,
the source, caller and independent reference are respectively
`test/cases/<selector>/Example.hs`, `Case.hs` and `Reference.hs` in that directory.

| Selector | Probe | Value | Generated/reference cost |
|---|---|---:|---|
| `polymorphic-shared-top-level-value` | `fixed-cross-type-calls` | 5 | 4 / 3 |
| `polymorphic-shared-top-level-value` | `cross-type-shared-calls` | -2 | 16 / 9 |
| `polymorphic-shared-top-level-value` | `conditional-bools-then-ints` | -2 | 12 / 7 |
| `polymorphic-shared-top-level-value` | `repeated-boolean-calls` | False | 8 / 5 |
| `shared-top-level-across-exported-calls` | `fixed-exported-calls` | 30 | 4 / 3 |
| `shared-top-level-across-exported-calls` | `interleaved-exported-calls` | 100 | 16 / 9 |
| `shared-top-level-across-exported-calls` | `repeated-first-call` | 66 | 8 / 5 |
| `top-level-function-sharing` | `fixed-top-level-sharing` | 20 | 5 / 4 |
| `top-level-function-sharing` | `uncached-top-level-calls` | 40 | 10 / 7 |
| `top-level-function-sharing` | `cached-top-level-call` | 40 | 5 / 4 |
| `top-level-function-sharing` | `independent-export-calls` | 20 | 4 / 3 |

All listed values match at the common boundary sample. The references retain
source top-level work across calls; for example, the top-level-function reference
retains one thunk outside ordinary caller allocation roots and counts its work
once. This is a source-global scope conflict, not permission to insert a hidden
runtime initialization protocol.

## Intended behavior and acceptance

An approved solution must preserve the unchanged independent reference, entry
ticks, laziness and ordinary exported interfaces, and match retained sharing
across exported calls and type instantiations. Source-global support needs an
explicit scope decision; no runtime change is authorized by this classification.
Keep all guards before ordinary caller builds and comparisons. Verify boundary
and seeded samples and existing optimized postchecks before reviewed promotion.

## Reproduce

Prepare the runtime using `patches/README.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=polymorphic-shared-top-level-value
sh scripts/stack-test.sh --test-arguments=shared-top-level-across-exported-calls
sh scripts/stack-test.sh --test-arguments=top-level-function-sharing
```
