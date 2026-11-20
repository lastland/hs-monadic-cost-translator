# Track open Foldable method construction rejections

Status: needs-triage
Type: task

## Cases and observed diagnostics

Five exact cases in the [audit map](../map.md) fail the complete-output
construction guard. For each selector, the source, caller and independent
reference are `test/cases/<selector>/Example.hs`, `Case.hs` and `Reference.hs`
in that directory.

| Selector | Rejected selected method |
|---|---|
| `direct-concatMap-maybe` | `Generated.concatMapT` |
| `direct-foldl-pair-unused-first-field` | `Generated.foldlT` |
| `direct-foldr-maybe` | `Generated.foldrT` |
| `direct-foldr-nothing-unused-callback` | `Generated.foldrT` |
| `generated-properties` | `Generated.foldrT` |

Each reports `unknown executable method/default dependency` with
`given FoldableT t` evidence. This is not a comparison result or an ordinary
caller build failure. The partial `head` warning in concatMap is not the
rejecting condition.

## Intended behavior and scope

An approved class/default-method design must establish executable closure for
the selected methods before accepting construction. Preserve unused callbacks,
unused pair fields, finite observations and exact reference costs. Do not accept
an arbitrary open dictionary merely because a concrete call uses Maybe or pairs.

Open Foldable support may remain deferred under the approved scope of
[local issue #12](../../translator-poc/issues/12-user-defined-type-classes.md).
This issue records failures, not authorization to broaden that scope. Keep the
independent references and all assertions unchanged.

## Acceptance and reproduction

After approved support, require complete-output guards, ordinary caller builds,
boundary/seeded comparisons and existing optimized postchecks, in that order.
Prepare the runtime using `patches/README.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=direct-concatMap-maybe
sh scripts/stack-test.sh --test-arguments=direct-foldl-pair-unused-first-field
sh scripts/stack-test.sh --test-arguments=direct-foldr-maybe
sh scripts/stack-test.sh --test-arguments=direct-foldr-nothing-unused-callback
sh scripts/stack-test.sh --test-arguments=generated-properties
```
