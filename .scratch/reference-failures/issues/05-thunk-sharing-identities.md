# Resolve thunk sharing identity and cost mismatches

Status: needs-triage
Type: task

## Cases and evidence

The [audit map](../map.md) names three independent failures. Callers and unchanged
references are `test/cases/<selector>/Case.hs` and
`test/cases/<selector>/Reference.hs`.

| Selector | Source |
|---|---|
| `manual-thunk-matching` | `examples/ThunkMatching.hs` |
| `thunk-matching-sharing` | `test/cases/thunk-matching-sharing/Example.hs` |
| `thunk-sharing` | `test/cases/thunk-sharing/Example.hs` |

Complete-output guards and ordinary caller compilation succeed. Comparison then
reports identity **value** mismatches as well as costs at the boundary sample:

- `manual-thunk-matching`: `dictionaries/repeated` returns 360 at generated cost
  48 versus 26; `polymorphic/repeated` returns 0 at 20 versus 13.
  `dictionaries/identities-and-local-freshness` returns
  `[False,False,False,False,True,True,True,True,True]` instead of nine `True`s.
  `polymorphic/identities-and-local-freshness` returns
  `[False,False,True,True,True]` instead of five `True`s.
- `thunk-matching-sharing`: `poly/top-repeated` returns 0 at 24 versus 9.
  `identities/top-polymorphic` returns `[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]`
  instead of sixteen zeroes. `identities/dictionaries` returns
  `[0,1,2,3,4,5,6,7,8,9,10,11]` instead of `[0,0,0,0,0,0,0,0,0,1,1,1]`;
  `identities/wide-dictionary-only-keys` returns `[0,1,2,3,4,5]` instead of
  `[0,0,0,0,1,1]`. Numeric case/guard and scoped case/guard probes return 20
  at 3 versus 2; local-scoped probes return 20 at 4 versus 3.
  `numeric/repeated-observations` returns 480 at 19 versus 9. Wide bool/int/integer
  probes return 20 at 3 versus 2; wide repeated observations return 240 at
  9 versus 5; defaulted repeated shared dictionary returns 40 at 16 versus 9.
- `thunk-sharing`: `same/source` returns 20 at 3 versus 2;
  `shared/fixed-repeated` returns 40 at 6 versus 4;
  `shared/repeated-demands` returns 160 at 6 versus 4;
  `shared/repeated-calls` returns 160 at 24 versus 10.
  `shared/preparation-resources` returns `([False,False,True],2)` instead of
  `([True,True,True],2)`, with no cost attached to that identity observation.

These cases exercise retained top-level identities, dictionary keys and local
freshness. Not every numeric discrepancy is thereby proved source-global:
local-scoped and one-observation ticks need separate accounting.

## Intended behavior and acceptance

Match actual reference identities and demanded costs, not merely printed
arithmetic values. Preserve local freshness, distinct dictionary partitions,
resource counts, laziness and ordinary exported interfaces. Keep references and
identity assertions unchanged. Source-global identity support remains a separate
scope decision; this issue authorizes no runtime cache or initialization scheme.
Keep complete-output guards and ordinary caller builds before all comparisons,
including boundary/seeded samples and existing optimized postchecks.

## Reproduce

Prepare the runtime using `docs/runtime.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=manual-thunk-matching
sh scripts/stack-test.sh --test-arguments=thunk-matching-sharing
sh scripts/stack-test.sh --test-arguments=thunk-sharing
```
