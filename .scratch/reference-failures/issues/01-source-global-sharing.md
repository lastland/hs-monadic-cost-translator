# Resolve source-global sharing cost differences

Status: needs-triage
Type: task

## Verified case

- Selector: `anf-scoped-renaming`
- Source: `test/cases/anf-scoped-renaming/Renamed.hs`
- Caller: `test/cases/anf-scoped-renaming/Case.hs`
- Independent reference: `test/cases/anf-scoped-renaming/Reference.hs`
- Verified implementation pin: `e4633c8d5fea09498c9d52d8be18217fe0afa4fb`
- Source SHA256: `8b885b286ceeec210b5302b2296e4ac6fe0fc6903d08d169e2029acf0f32dab9`

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

Initialize and patch the pinned runtime as documented in `patches/README.md`,
then run:

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test \
  --test-arguments=anf-scoped-renaming
```

The existing selector runs source variants in sorted order. `Renamed.hs` is
first, so its failure currently prevents the same command from reaching
`Shadowed.hs`. Classify that sibling independently in the new per-source
reference inventory.

## Intended behavior and scope

Keep the independent reference and its strict cost assertions unchanged.
Preserve source values, local sharing, dictionary partitions, laziness and
ordinary exported function interfaces.

Earlier scope decisions exclude source-global sharing work and identity from
the current implementation effort. This issue preserves the stricter reference
failure for a later decision; it does not authorize a runtime change, evaluator,
new cache, initialization protocol or extra caller constraint.

Related unresolved clarification:
[issue #13](https://github.com/lastland/hs-monadic-cost-translator/issues/13#issuecomment-7034721010).

## Acceptance

An approved implementation must pass the unchanged reference comparison through
the complete shared lifecycle. Verify boundary and seeded inputs, both source
variants independently, and O0/O2 behavior before promoting a deferred case into
the passing suite.

An explicit future policy decision may instead resolve the scope conflict.
Until then, report the case as a known failure rather than weakening its oracle
or treating the passing partition as full reference-suite success.

## Comments

- The initial issue covers only the source variant verified above. Add other
  cases only after their fresh results establish the same failure.
