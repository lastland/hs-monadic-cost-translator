# Resolve source-global sharing cost differences

Status: needs-triage
Type: task

## Verified cases

- Selector: `anf-scoped-renaming`
- Source: `test/cases/anf-scoped-renaming/Renamed.hs`
- Independently verified sibling: `test/cases/anf-scoped-renaming/Shadowed.hs`
- Caller: `test/cases/anf-scoped-renaming/Case.hs`
- Independent reference: `test/cases/anf-scoped-renaming/Reference.hs`
- Verified implementation pin: `e4633c8d5fea09498c9d52d8be18217fe0afa4fb`
- Source SHA256: `8b885b286ceeec210b5302b2296e4ac6fe0fc6903d08d169e2029acf0f32dab9`

The earlier pin and source hash above describe the original Renamed receipt.
The authoritative audit at `e5d4652cd08a4cb4a02d1b9fbc9ac22d67026268`
independently executes both variants and observes the same three mismatches
below for each. Both remain open; neither is inferred from its sibling.
See [the audit map](../map.md) for provenance and the exact classification.

The complete-output checks and ordinary caller compilation succeed. The
reference comparison then reports matching values but different demanded costs:

| Probe | Returned value | Generated cost | Reference cost |
|---|---:|---:|---:|
| `factory/int-repeated` | 8 | 24 | 17 |
| `factory/integer-repeated` | 16 | 32 | 18 |
| `factory/mixed-dictionaries` | 24 | 56 | 35 |

These results come from the boundary sample with `number = 5`, `other = 0`,
`numbers = [1,2,3]`, `width = 3`, `repeats = 4`, both demand flags false and
`variant = 0`. The retained failing input was checked byte-for-byte against
`Renamed.hs`.

## Reproduce

Initialize the pinned runtime as documented in `docs/runtime.md`,
then run:

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test \
  --test-arguments='--known-failures anf-scoped-renaming'
```

The selector visits both source variants independently, in sorted order, and
returns failure while either reference comparison fails. To run one source:

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test \
  --test-arguments='--known-failures anf-scoped-renaming:test/cases/anf-scoped-renaming/Shadowed.hs'
```

The original audit predates per-source CLI selection. Its separate executions
remain historical evidence; the current CLI no longer stops at the first
ordinary comparison failure. Setup failures still stop execution immediately.

## Intended behavior and scope

Keep the independent reference and its strict cost assertions unchanged.
Preserve source values, local sharing, dictionary partitions, laziness and
ordinary exported function interfaces.

Earlier scope decisions exclude source-global sharing work and identity from
the current implementation effort. This issue preserves the stricter reference
failure for a later decision; it does not authorize a runtime change, evaluator,
new cache, initialization protocol or extra caller constraint.

The [issue #13 closure decision](https://github.com/lastland/hs-monadic-cost-translator/issues/13#issuecomment-7054311100)
resolves the comparison clarification. In these two variants, the existing
source-global exclusion covers only the tick differences for
`factory/int-repeated`, `factory/integer-repeated`, and
`factory/mixed-dictionaries`. Keep their probes and expected costs unchanged.
All values must match; comparisons, parameter scoping and fresh locals must
match on both values and ticks. The retained global-cost failures do not reopen
the completed comparison issue.

## Acceptance

An approved implementation must pass the unchanged reference comparison through
the complete shared lifecycle. Verify boundary and seeded inputs, both source
variants independently, and O0/O2 behavior before promoting a deferred case into
the passing suite.

An explicit future policy decision may instead resolve the scope conflict.
Until then, report the case as a known failure rather than weakening its oracle
or treating the passing partition as full reference-suite success.

## Comments

- The fresh authoritative audit adds `Shadowed.hs` after independent execution.
  Preserve complete-output guards and ordinary caller compilation before every
  comparison, including the separate known-failure run.
