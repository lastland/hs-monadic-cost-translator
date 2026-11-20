# Higher-kinded datatype field audit — implemented

Status: ready-for-human

## Resolution

Addressed by commit `91e2d08`: kind-correct representation, conversion, and
demand generation shipped with the translator implementation and passed the
full integration suite. Status left for the maintainer to close.

The earlier kind errors were generator defects, not evidence of impossibility
or grounds for reducing scope. Compiled probes and generated-code acceptance
tests now pass without changing the external runtime API.

`Cost.Translate.Kinds` generates:

- `RepOf`/`DemOf`, backed by families with an explicit kind index and standalone
  kind signatures. At Type they use runtime Rep/Dem; at constructor kinds they
  map builtin and local datatype constructors and partial applications.
- ReprK/DemandK requirements. At Type these require the existing runtime
  classes; at arrow kinds quantified application constraints lift the same
  obligations and carry the application mapping equalities.
- GeneratedK, which lifts the existing bounded structural-demand interface.
- ShapeK, shared by demand Eq, Show and Arbitrary instance generation.
- Pure dictionary witnesses. Recursive methods explicitly select the application
  dictionary before using its mapping equalities; no tick or thunk is added.

The frontend uses GHC binder visibility to separate kind arguments from ordinary
datatype parameters. It preserves inferred binder kinds, including a source
`Higher :: (k -> Type) -> k -> Type`. Generated parser options and language
pragmas are synchronized for the required standard GHC extensions.

The datatype emitter now has named per-constructor information and separate
renderers for conversion, demand observation, bounded generation and shrinking.
This replaced the incorrect per-parameter Repr f/Demand f contexts; it does not
merely suppress their errors or specialize a source parameter to observed uses.

## Verified cases

| Case | Result |
| --- | --- |
| `data Higher f a = Higher a (f a)` | Translates, typechecks, value 5, cost 3 |
| `data Fix f = In (f (Fix f))` | Translates, typechecks, value 5, cost 3 |
| `data Higher f a = Higher (f a)` | Inferred kind polymorphism retained; value 5, cost 3 |
| `data Nest f = Nest (Maybe (f (Nest f)))` | Nested recursive conversion and finite exact-cost demands pass |
| `data Mixed f a = Leaf a \| Branch (f (Mixed f [a]))` | Non-regular conversion, demands and shrinks pass |
| Custom local `Layer` as a constructor parameter | Conversion and generated demands pass |
| `Higher Fix []` | A constructor taking a constructor argument converts and supports demands/shrinks |
| Index kinds | Natural, symbol, character and promoted source constructor/operator indices pass |
| Infinite `Fix []` producer | 100 bounded generation trials; compatible demands/shrinks; exact node-entry costs |
| Budget zero | Skips a failing field without observing it |
| Conversion | Explicit demand observations add zero ticks |

Existing nested list/pair parameter tests also pass. No regular-datatype
restriction, extra source annotations, external-runtime extension or artificial
list conversion has been introduced.

## Verification commands

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test \
  --test-arguments='higher-kinded-datatype-field higher-kinded-recursive-field higher-kinded-polykinded-field higher-kinded-demands'
sh scripts/stack-test.sh --stack-yaml integration/stack.yaml cost-translate-integration:test:runtime-higher-kinded
```

The independent handwritten gate is `test/RuntimeHigherKindedGate.hs`. It was
compiled/executed before integrating the convention and remains distinct from
the generated-code tests. The final full integration suite passed.

Generated-code fixtures from the final run:

- Non-recursive: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/cost-translate-e2e--5f3ba8890cc1c321`
- Recursive: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/cost-translate-e2e--5ac090ce74b3d87f`
- Kind-polymorphic: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/cost-translate-e2e--6ff835d5c4b471da`
- Conversion/demands: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/cost-translate-higher-demands--79c5f9c6e0198c5d`

The remaining step for this issue is the parent's independent review.
