# Reference failure audit map

## Notes

Authoritative implementation pin: `e5d4652cd08a4cb4a02d1b9fbc9ac22d67026268`.
The unpublished audit `report.json` has SHA256
`575086a9ad82d2aa60d3596aa4fe8c35a5d6c125fd41ac0b4c8c3a9473f17b96`.
Its companion `known-failure-proposal.json` retains each exact selector/source,
failure reason, execution result and artifact receipt. These are unpublished
audit artifacts, not repository files or portable reproduction executables.

The audit accounts for exactly 177 reference variants: 144 PASS, 33 FAIL
(13 construction, 20 value/cost), 0 BLOCKED. Source bytes remained unchanged.
All 132 historical passing cases still pass. This map lists only the 33 failures;
it does not classify passing siblings as failures.

The parent explicitly approved this initial classification under the user policy:
default passing suite plus a separate known-failure run with unchanged assertions
and nonzero exit while failures remain. New failures never enter automatically;
recovery requires a reviewed inventory change. No candidate fix is integrated
merely by recording it here.

All value/cost examples use the boundary sample `number = 5`, `other = 0`,
`numbers = [1,2,3]`, `width = 3`, `repeats = 4`, both demand flags false,
`variant = 0`. Comparison-stage failures already passed complete-output guards
and ordinary caller compilation. Construction failures did not reach comparison.
Every future run must preserve these guards before caller builds and comparisons,
the independent references, seeded samples and existing optimized postchecks.

## Exact selector/source to issue mapping

Each row is one case, identified by both columns, not the selector alone.
Issue links are relative to this map; consumers should resolve them under
`.scratch/reference-failures/`.

| Selector | Source | Issue |
|---|---|---|
| `anf-scoped-renaming` | `test/cases/anf-scoped-renaming/Renamed.hs` | [01](issues/01-source-global-sharing.md) |
| `anf-scoped-renaming` | `test/cases/anf-scoped-renaming/Shadowed.hs` | [01](issues/01-source-global-sharing.md) |
| `constrained-sharing-all-dictionaries` | `test/cases/constrained-sharing-all-dictionaries/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-constructor-dictionaries` | `test/cases/constrained-sharing-constructor-dictionaries/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-different-dictionaries` | `test/cases/constrained-sharing-different-dictionaries/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-generic-caller` | `test/cases/constrained-sharing-generic-caller/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-nested-allocations` | `test/cases/constrained-sharing-nested-allocations/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-same-dictionary` | `test/cases/constrained-sharing-different-dictionaries/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `constrained-sharing-unconstrained-parameter` | `test/cases/constrained-sharing-unconstrained-parameter/Example.hs` | [02](issues/02-constrained-sharing-costs.md) |
| `datatypes-and-demands` | `test/cases/datatypes-and-demands/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `direct-concatMap-maybe` | `test/cases/direct-concatMap-maybe/Example.hs` | [07](issues/07-open-foldable-construction.md) |
| `direct-foldl-pair-unused-first-field` | `test/cases/direct-foldl-pair-unused-first-field/Example.hs` | [07](issues/07-open-foldable-construction.md) |
| `direct-foldr-maybe` | `test/cases/direct-foldr-maybe/Example.hs` | [07](issues/07-open-foldable-construction.md) |
| `direct-foldr-nothing-unused-callback` | `test/cases/direct-foldr-nothing-unused-callback/Example.hs` | [07](issues/07-open-foldable-construction.md) |
| `generated-properties` | `test/cases/generated-properties/Example.hs` | [07](issues/07-open-foldable-construction.md) |
| `higher-kinded-demands` | `test/cases/higher-kinded-demands/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `higher-kinded-polykinded-field` | `test/cases/higher-kinded-polykinded-field/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `manual-scoped-annotations` | `examples/ScopedAnnotations.hs` | [03](issues/03-scoped-factory-costs.md) |
| `manual-thunk-matching` | `examples/ThunkMatching.hs` | [05](issues/05-thunk-sharing-identities.md) |
| `mutually-recursive-captured-helpers` | `test/cases/mutually-recursive-captured-helpers/Example.hs` | [08](issues/08-generic-comparison-construction.md) |
| `nested-pair-and-maybe-fields` | `test/cases/nested-pair-and-maybe-fields/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `polymorphic-checked-payload-kinds` | `test/cases/polymorphic-checked-payload-kinds/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `polymorphic-dependent-payload-kinds` | `test/cases/polymorphic-dependent-payload-kinds/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `polymorphic-shared-top-level-value` | `test/cases/polymorphic-shared-top-level-value/Example.hs` | [04](issues/04-top-level-sharing-costs.md) |
| `polymorphic-vacuous-quantification` | `test/cases/polymorphic-vacuous-quantification/Example.hs` | [06](issues/06-demand-data-construction.md) |
| `scoped-factory-case-shadow-int` | `test/cases/scoped-factory-case-shadow/Example.hs` | [03](issues/03-scoped-factory-costs.md) |
| `scoped-factory-case-shadow-integer` | `test/cases/scoped-factory-case-shadow/Example.hs` | [03](issues/03-scoped-factory-costs.md) |
| `scoped-factory-guard-shadow-int` | `test/cases/scoped-factory-guard-shadow/Example.hs` | [03](issues/03-scoped-factory-costs.md) |
| `scoped-factory-guard-shadow-integer` | `test/cases/scoped-factory-guard-shadow/Example.hs` | [03](issues/03-scoped-factory-costs.md) |
| `shared-top-level-across-exported-calls` | `test/cases/shared-top-level-across-exported-calls/Example.hs` | [04](issues/04-top-level-sharing-costs.md) |
| `thunk-matching-sharing` | `test/cases/thunk-matching-sharing/Example.hs` | [05](issues/05-thunk-sharing-identities.md) |
| `thunk-sharing` | `test/cases/thunk-sharing/Example.hs` | [05](issues/05-thunk-sharing-identities.md) |
| `top-level-function-sharing` | `test/cases/top-level-function-sharing/Example.hs` | [04](issues/04-top-level-sharing-costs.md) |

## Decisions so far

- `higher-kinded-demands` is also explicitly promoted after the stock evidence
  checker learned to retain the exact builtin empty constraint. Its unchanged
  reference passes complete-output guards, ordinary callers and O0/O2 boundary
  cases plus 200 randomized comparisons in the integrated runner. The current
  catalog contains 27 deferred cases; the historical audit table remains intact.
- Five cases are explicitly promoted to the passing partition after integrated
  complete-output, ordinary caller and O0/O2 reference verification:
  `higher-kinded-polykinded-field`, `polymorphic-checked-payload-kinds`,
  `polymorphic-dependent-payload-kinds`, `polymorphic-vacuous-quantification`
  and `mutually-recursive-captured-helpers`. The historical 33-row audit table
  above is preserved; that checkpoint left 28 deferred cases. No assertions or
  fixture/reference bytes changed.
- Issues 01–05 preserve cost and identity failures separately from construction
  failures in issues 06–08. Grouping fixtures by evidenced behavior is not a
  claim that every extra tick has one common cause.
- Issue 01 now includes independently audited `Shadowed.hs`; its original
  `Renamed.hs` receipt remains available.
- Existing selector commands in the child issues are valid at the baseline.
  Multi-variant selectors can stop at their first failure: in particular,
  `anf-scoped-renaming` stops at `Renamed.hs` before `Shadowed.hs`. The audit
  selected sources independently. Portable per-source CLI commands remain
  pending; `futurePerSourceSelection` in the proposal is not implemented syntax.

## Fog

- Retained top-level sharing/identity support still needs an explicit scope
  decision; local allocation and observation costs must not be assumed global.
- Stock Eq/Show candidate `e577` and closed-local candidate `e34` are not
  integrated into this baseline. Growing-recursion closure remains unsupported,
  not inherently unsafe.
- Open Foldable work may remain deferred under the existing general-typeclass
  scope decision. A bounded inferred-local comparison candidate is implemented
  and separately reviewed, but is not integrated here; those results do not
  establish that it closes issue 08's generic equality rejection.
