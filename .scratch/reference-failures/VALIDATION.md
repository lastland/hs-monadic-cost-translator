# Reference-suite split verification

Validated code checkpoint: `3bbeaac5568f63150d4d008d136606b728b6d746`.

## Final results

| Command | Result |
|---|---|
| `sh scripts/stack-test.sh cost-translate --ghc-options='-Wall -Werror'` | Exit 0; 149 passing reference variants and infrastructure checks |
| `sh scripts/stack-test.sh cost-translate --test-arguments=--known-failures --ghc-options='-Wall -Werror'` | Exit 1; all 28 deferred variants visited and failed |
| `sh scripts/validate.sh` | Pass |

The default log is `stack-test.IOh1ce`; the known-failure log is
`stack-test.CPDWau`. Both are retained unpublished validation artifacts.
Their exact per-source keys were checked against discovery and the original
177-variant audit. The two partitions are disjoint and cover all 177 variants.
No passing case was omitted or automatically quarantined.

The default run includes all infrastructure groups and the four explicit crash
controls: signal and exit-1 panic during ordinary candidate checking, and signal
and panic during the missing-runtime negative test. The genuine missing-runtime
negative test still passes.

## Explicit promotions

Five former construction failures now pass unchanged ordinary references:

- `higher-kinded-polykinded-field`
- `polymorphic-checked-payload-kinds`
- `polymorphic-dependent-payload-kinds`
- `polymorphic-vacuous-quantification`
- `mutually-recursive-captured-helpers`

Before promotion, the integrated 33-case deferred run visited every original key,
reported exactly these five recoveries, and retained 28 failures with exit 1.
Every recovery passed complete-output guards, ordinary caller builds, and O0/O2
boundary cases plus 200 randomized value/cost comparisons. Promotion removed
only these five catalog entries. Source, caller and independent-reference trees
remain unchanged from the audited baseline.

## Review and regression evidence

All substantiated Standards and Spec findings were addressed, including low
severity findings. Final pinned reports verify zero findings for the crash
corrections, test consolidation, integration, documentation correction and
explicit promotion, within each declared review scope.

Public CLI RED/GREEN receipts include:

- Exit-1 compiler panic: `stack-test.yep7HA` RED, then `stack-test.UJobN6` GREEN.
- Missing-runtime crash false positive: `stack-test.dCPI6j` RED, then
  `stack-test.JK82yc` GREEN.
- Consolidated signal/panic controls: `stack-test.f8LyRh` GREEN.

## Remaining limits

The passing partition is green; the complete inventory is not. The 28 deferred
cases retain their original strict cost, value, identity or construction
assertions and local issue links. The historical audit remains in
[`map.md`](map.md); the current catalog is
[`test/ReferenceSelection.hs`](../../test/ReferenceSelection.hs).

Custom-CPP provenance remains separately unresolved in
[`01-custom-wrapper-inputs.md`](../cpp-provenance/issues/01-custom-wrapper-inputs.md).
No custom-wrapper admission or general typeclass/growing-recursion support is
claimed by this test split.
