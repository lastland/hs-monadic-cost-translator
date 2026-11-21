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
freshness. The original mismatch list alone did not establish the binding scope
of every discrepancy. The scope audit below traces the originating bindings and
separately checks the genuinely local observations.

## Intended behavior and acceptance

Match actual reference identities and demanded costs, not merely printed
arithmetic values. Preserve local freshness, distinct dictionary partitions,
resource counts, laziness and ordinary exported interfaces. Keep references and
identity assertions unchanged. Source-global identity support remains a separate
scope decision; this issue authorizes no runtime cache or initialization scheme.
Keep complete-output guards and ordinary caller builds before all comparisons,
including boundary/seeded samples and existing optimized postchecks.

## Scope accounting

The [global-cost decision in #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2#issuecomment-6972277826)
and [global-identity decision in #4](https://github.com/lastland/hs-monadic-cost-translator/issues/4#issuecomment-6970326527)
defer these global requirements. The #4 report explicitly includes the two Int
fields of one `same` result failing to alias. A single function invocation does
not make its uses of the top-level `shared` binding source-local.

- In `test/cases/thunk-sharing/Example.hs`, `same` and `different` use top-level
  `shared`; the genuinely local binding is `y` inside `local`.
- In `test/cases/thunk-matching-sharing/Example.hs`, `localScoped` defines a local
  function `go`, but `go` still reads top-level `shared`. The numeric, wide and
  top-polymorphic mismatches likewise originate at top-level bindings.
- In `examples/ThunkMatching.hs`, the failing identity-vector components compare
  top-level `shared` or `sharedEmpty`. Numeric components 6–9 and polymorphic
  components 3–5 check local sharing, dictionary separation and freshness; those
  components match.

A bounded audit at the `3008833` source checkpoint compared every original probe
over all 12 boundaries and 200 seeded inputs at both O0 and O2, without stopping
at or truncating the global mismatches. The actual local probes and the local
identity components above match throughout. All three original fixture
comparisons still fail.

The manual fixture mixes local and global calls in its repeated-cost probes.
Supplementary ordinary callers therefore invoked only its existing
`localDictionariesM` and `localPolymorphicM` exports, against the complete unchanged
generated module and independent reference. At each optimization, all 124
supplementary checks match the reference and independently derived expectations:
numeric full demand costs 3, repeated demands on one result still cost 3, and
separate calls cost `3*r`; the corresponding polymorphic costs are 2, 2 and
`2*r`. Partial demands, skipped fields, ignored bottom arguments, dictionary
partitions and fresh calls also match.

No independently local mismatch was found in these observations. This is bounded
scope evidence, not an exhaustive correctness proof or a replacement passing
suite. All 130 original artifacts remained unchanged. No production reuse rule,
runtime change, assertion change or catalog promotion follows from this audit;
the strict global counterexamples remain deferred and visible.

## Reproduce

Prepare the runtime using `docs/runtime.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=manual-thunk-matching
sh scripts/stack-test.sh --test-arguments=thunk-matching-sharing
sh scripts/stack-test.sh --test-arguments=thunk-sharing
```

## Bounded feasibility evidence

Throwaway branch `constructor-field-sharing-prototype-probe`, commit `c6e696e`,
retains a native O0/O2 probe and its receipt under
`.scratch/constructor-field-sharing-prototype/`. No probe implementation is
integrated into the translator.

For one `sameM` call, manually retaining one native thunk gives sum 20, cost 2
and equal field identities, matching the unchanged reference. Complete
structural/construction checks and ordinary callers pass. The hand-edited
candidate is not claimed to retain authenticated publication provenance.
Two calls still cost 4 instead of 3 and retain different identities. The
unchanged full caller remains RED, including cross-export sharing.

A separate full monomorphic source variant confirms the same bounded result.
This does not establish an automatic occurrence planner or dictionary
equivalence for the original polymorphic `shared`. Existing specialization
checks reconstruct types and predicates, not selected dictionary identities.
No constrained result-type-only merging, runtime extension or test waiver is
approved by this evidence.
