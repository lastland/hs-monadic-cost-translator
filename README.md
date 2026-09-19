# Haskell cost-monadic translator

A proof-of-concept translator from one pure Haskell module to explicit lazy
thunks and cost operations in the external Memorist runtime. It uses GHC to
parse, resolve names and infer types, then translates renamed source syntax
before optimization. Every output emitter constructs GHC `GhcPs` syntax through
`Cost.Translate.Output`. `Pretty` renders the completed module once; it does not
reparse assembled source or expression fragments. GHC then typechecks the
candidate before the CLI publishes it.

**Implementation checkpoint:** higher-kinded representation, conversion and
demand generation are implemented, and the complete integration suite passes.
The direct `FoldableT` interface preserves inferred container polymorphism.
Generated conversions require exported source constructors, as approved by the
user. Final parent code review and commit are pending. See
[the checkpoint](.scratch/translator-poc/IMPLEMENTATION-STATUS.md) and
[the audit](.scratch/translator-poc/issues/05-higher-kinded-datatype-audit.md).

## Build and external runtime setup

The compiler is pinned by Stack **LTS 24.22 / GHC 9.10.3**:

```sh
stack build
stack run cost-translate -- Example.hs Generated.hs
```

The translator package does **not** depend on MemoristANT or bundle its source.
Generated modules require an existing GHC environment exposing
`MemoristANT` and `QuickCheck`. The CLI does not install either package or accept
a custom runtime path. It invokes `ghc` from `PATH`; use the same pinned compiler
as the translator. The root Stack project registers the locally supplied
external checkout at `hs-pbt/` (ignored by Git) and QuickCheck 2.17.1.0, so
GHC's environment under `stack run` can typecheck generated candidates; the
runtime checkout must exist locally for these commands.

The separate integration project runs the acceptance test suites. It is a
development/test environment, not a translator dependency, and assumes the
external runtime already exists:

```sh
stack --stack-yaml integration/stack.yaml test
```

Its `scanlM` must include the authorized demand-laziness correction recorded in
[scanl-demand-fix.patch](.scratch/translator-poc/scanl-demand-fix.patch).
The runtime regression tests verify both delayed input-spine forcing and delayed
callback forcing.

## CLI and generated interface

```sh
stack run cost-translate -- SOURCE.hs OUTPUT.hs
```

Use an uppercase Haskell module basename for the output, such as `Generated.hs`.
Example sources for trying the CLI live in [`examples/`](examples/); see
[`examples/README.md`](examples/README.md) for per-file walkthroughs with
verified values and costs.
The tool writes `Generated.hs.log`, including source-located resolved function
identities, syntactic arities, supplied argument counts, adapters and compiler
diagnostics. Failed unsupported translations retain this log. A failed candidate
does not replace the output file; successful publication occurs by rename after
GHC typechecking. An older successfully generated output can therefore remain
after a later failure: check the command's exit status and log.

Diagnostics distinguish source syntax/type errors, unsupported source, missing
target-package setup, generated-code type errors, and an invalid output module
name. The generated-code categories indicate a translator defect or unhandled
implementation case, not a successful translation.

For example, `Example.hs`:

```haskell
module Example where

double x = x + x
shared x = let y = double x in y + y
```

The generated `sharedM` accepts a thunk and returns `MemoristM`. In the runtime
environment, a handwritten test can use:

```haskell
correct (fromPure1 sharedM (5 :: Int)) (20 :: Int)
-- True

infcost (fromPure1 sharedM (5 :: Int)) fullDemand
-- Cost 2: one shared invocation, one double invocation
```

If a module has top-level value bindings, the output exports a `Bindings`
record and `initialize :: MemoristM Bindings`. Initialize once per evaluation
and reuse its fields across calls:

```haskell
do bindings <- initialize
   a <- fromPure1 (firstM bindings) (5 :: Int)
   b <- fromPure1 (secondM bindings) (5 :: Int)
   pure (a + b)
```

This keeps top-level thunks shared between exported calls. Unconstrained
polymorphic values reserve one runtime name and retain sharing across type
instantiations. Dictionary-constrained values instead use a stable subtree per
instantiation of the generalized variables occurring in their constraints.
Repeated uses of the same constrained instantiation share; variables absent
from the constraints do not split that sharing. For example, `Num a => (a,[b])`
shares across changes to `b` while distinguishing different `a` dictionaries.

The generated code obtains the required `Typeable` evidence in affected value
and caller signatures. Keys structurally encode qualified type constructors,
kind arguments and applications, rather than hashing or printing short names.
Reserved name delimiters isolate the instance subtrees from ordinary allocations
and from each other, including nested shared bindings. GHC type evidence retains
source annotations and defaulted instantiations. Source annotations are not
required solely for translation.

The independent polymorphic-sharing probe still checks an unconstrained binding
at `Int` and `Bool`. Generated regressions additionally check dictionary-sensitive
values: the overflow-sensitive Int/Integer example returns `(1,2)` at cost 5,
while repeated Int uses share their computation. These conventions use the
external named-thunk runtime's `lazy`/`eval` operations.

## Implemented coverage

The end-to-end tests exercise:

- Inferred polymorphic function signatures and shared polymorphic values.
- Named recursive functions, mutually recursive local helpers, captured values,
  non-recursive `let`/`where` values, and top-level shared initialization.
- Ordered equations, constructor/list/literal/as/wildcard/variable patterns,
  `case`, Boolean guards, pattern guards and `let` guards. Literal patterns
  include numbers, characters and strings; a string literal pattern decomposes
  into its character sequence on the representation, forcing cell by cell and
  stopping at the first mismatch like the source comparison.
- Record fields are extracted by their declaration positions, but supplied
  record subpatterns are tested in source order. A mismatch can therefore skip
  a later failing field even when that field was declared earlier.
- Arithmetic and comparisons on scalar values, and short-circuit `&&`/`||`.
  `Char` is a scalar primitive throughout: fields, conversion, demands and
  `Eq`/`Ord` comparisons. The external runtime has no Char instances, so each
  generated module carries local identity conversions (`Rep Char = Char` and
  the demand counterparts) instead of changing the runtime.
- Suspended `undefined`, literal-message `error`, and demanded match failures.
- Named function arguments, captured local callbacks, composition, and direct
  partial application at multiple argument counts, including four-argument
  functions. Adapters do not add ticks.
- Prefix `(.)`, applied compositions and partial applications of compositions.
  Captured operands and intermediate thunks are shared across repeated uses of
  the resulting callback, including nested compositions. The right operand may
  have any positive arity: `(map . add) x` supplies the partially applied
  `add x` as the mapped callback, with `add` ticking once per demanded element
  and no tick for the partial application itself.
- Lazy algebraic datatype representations, records, selectors, infix
  constructors, lists, pairs and `Maybe`. Every ordinary field is thunked,
  including scalar fields.
- Nested recursive parameters such as `Nested [a]` and `Nested (a,a)`; there is
  no blanket regular-recursion restriction.
- Higher-kinded fields (`Higher f a`), recursive constructor parameters (`Fix f`),
  and inferred kind polymorphism. Tests include nested applications, custom
  constructors, constructors taking constructor arguments, and non-regular
  recursion through a higher-kinded field.

### Resolved library bindings

Only resolved imported identities select replacements. Same-named source
definitions are translated normally.

Generated value, constructor, type and demand-helper names use disjoint naming
forms for identifiers and encoded operators. Names that resemble operator
encodings remain distinct, and record selectors participate in Prelude hiding.

| Source operation | External implementation |
| --- | --- |
| `map` | `Memorist.Example.List.mapMM` |
| `foldr`, `foldl` | Direct `FoldableT.foldrT` / `foldlT`; list instances delegate to `foldrM` / `foldlM` |
| `concatMap` | `FoldableT.concatMapT`; list instance delegates to `concatMapM` |
| `filter` | `filterM` |
| `zipWith` | `zipWithM` |
| `scanl` | `Memorist.Example.ListInf.scanlM` |
| `head`, `tail` | `headM`, `tailM` |
| Imported `Memorist.Example.NewtonRaphson.repeatly` | `repeatlyM`, with an unticked callback adapter |
| `(.)` | Generated unticked adapter |

Implicit and explicit Prelude imports are supported for these operations and
the implemented primitives. The only additional admitted import module is
`Memorist.Example.NewtonRaphson`, for `repeatly`; its other operations are not
automatically supported. There is no user-module dependency traversal.

An inferred source constraint `Foldable t` becomes `FoldableT t` over the
translated representations. `foldrT` and `foldlT` operate directly on lists,
`MaybeT`, and partially applied `PairT`; Maybe and pairs are not converted to
artificial lists. This is a fixed supported Prelude interface, not general
source typeclass translation. User-written classes and instances remain outside
the subset.

### Cost policy

Each translated source function invocation costs one tick, including trivial
wrappers and local helpers. Generated record selectors also charge one tick.
Value bindings and constructor construction do not tick. Scalar primitives
retain the costs of their operands without adding entry ticks. Composition and
partial application do not tick, but invoked functions do.

Handwritten replacements retain their existing instrumentation. For example,
`concatMapM` delegates to instrumented `foldrM` and `appendM`; it is not a
single-tick primitive. `repeatlyM` has no entry tick, so its producer cells do
not add ticks; demanded callbacks retain their own ticks. The corrected
`scanlM` ticks once per demanded scan invocation, including the seed cell.

Each direct Maybe/pair fold method charges one entry tick, then retains the
callback's ticks when invoked. A Nothing fold therefore costs its entry tick
without forcing the callback. The generic `concatMapT` method charges one entry
tick and uses direct `foldrT` plus the handwritten `appendM`; its list instance
delegates entirely to `concatMapM`. Noncommutative callback tests check fold
argument order as well as costs.

Primitive comparison validation rejects structured operands before generation:
the runtime's `Eq` instances for thunked structures compare thunk metadata and
are not source structural equality. Generated polymorphic Eq/Ord signatures
carry a `Scalar` constraint for the supported scalar domain. Desugared types
are inspected for this validation only; all instrumentation still uses renamed
syntax. Concrete inferred numeric literal types, variable instantiations and
explicit annotations are retained, including GHC's defaulting choices.

These are source-level costs, not GHC-optimized execution costs. A demanded
failure remains a failure. No cost result is promised for a failing computation.

## Datatypes and demands

For `Tree`, generation emits `TreeT`, `TreeD`, `Repr`, `Demand`, `LBoundDemand`,
`DemandArb` and the representation-demand marker instances. Conversion instances
refer to the original source module, which must remain available when compiling
the output. **The source datatype and all its constructors must be exported**
for generated conversions. With an explicit export list, use, for example:

```haskell
module Example (Box(..), make, readBox) where
data Box = Box Int
make x = Box x
readBox (Box x) = x
```

`module Example (Box, make, readBox) where` hides the constructor. The translator
now rejects it at the constructor's source location with an explicit unsupported
diagnostic: `generated conversions require the source module to export
constructor Box`. The check also detects partially exported constructor sets and
wholly private datatypes, before generating/typechecking a candidate. Source
exports are not modified. Future support for abstract source types is tracked in
[issue 04](.scratch/translator-poc/issues/04-private-constructor-conversion.md).

For an ordinary `Tree a`, the generated demand functions have the following
normalized types (the emitted signatures use the equivalent kind-aware
`GeneratedK` constraint and `DemOf` family):

```haskell
genTreeDemand    :: GeneratedDemand a => Int -> TreeT a -> Gen (TreeD (Dem a))
shrinkTreeDemand :: GeneratedDemand a => TreeT a -> TreeD (Dem a)
                -> [TreeD (Dem a)]
```

`arbDemandFrom` uses QuickCheck's size. The generator matches the observed
constructor and distributes a finite budget among fields. A field receives
`Skip` when its budget is exhausted; otherwise the default Skip:Need weighting
is 1:3. No skipped field is evaluated. Budgets conservatively charge for both
the demanded field and the observed structure. A finite budget bounds demand
generation, not the source program's time to satisfy that demand.

Compatible shrinkers keep observed constructors and either skip fields or
recursively shrink already demanded fields. For result-compatible testing, use
the result-indexed functions rather than standalone `arbitrary` demands:

```haskell
forAllShrink (genTreeDemand 20 result)
             (shrinkTreeDemand result) property
```

Custom generators and shrinkers can be supplied directly to `forAllShrink`;
no conflicting typeclass instances are needed. Standalone demand `Arbitrary`
instances are provided for the external class superclasses and have no default
shrinks; they are not a substitute for result-compatible generation.

### Higher-kinded support

These source declarations need no translator-specific annotation:

```haskell
data Higher f a = Higher (f a)
data Fix f = In (f (Fix f))
data Mixed f a = Leaf a | Branch (f (Mixed f [a]))
```

The output generates kind-indexed representation/demand mappings. At kind
`Type`, `RepOf` and `DemOf` use the existing runtime `Rep` and `Dem` families.
At constructor kinds, they map lists, Maybe, pairs and the selected module's
datatypes to their generated representations/demands, including partial
applications. Invisible kind arguments are distinguished from visible datatype
parameters; inferred kinds are retained in binders and signatures. Literal and
promoted indices are preserved at their index kinds.

Quantified application constraints lift the runtime interfaces through type
application. Explicit dictionary witnesses make the mapping equalities available
inside recursive conversion/demand methods. `GeneratedK` lifts the bounded
structural generator interface; `ShapeK` supplies Eq/Show/Arbitrary dictionaries
for demand values. These are generated adapters over the external interfaces,
not new source typeclass support or changes to the runtime API.
`ReprK` and `DemandK` are constraint synonyms; the application-witness classes
remain where quantified constraint heads require them.

Conversion adds no ticks, and every lazy field remains a thunk. The generated
tests verify finite budgets, observed-constructor compatibility, compatible
shrinks and exact producer-entry costs on infinite `Fix`/nested results. They
also exercise non-regular `Mixed` values, custom constructor mappings, natural,
symbol, character and promoted-constructor index kinds.

## Limitations and outstanding scope

The PRD excludes anonymous source lambdas, unrestricted higher-order source
definitions, recursive value bindings, lazy pattern bindings, explicit
irrefutable patterns, strict fields/patterns, `Strict`/`StrictData`, `seq`, strict
application, `newtype`, source classes/deriving, higher-rank types, polymorphic
recursion, and multi-module translation. Such constructs fail whole-module
translation. Other syntax outside the implemented subset (for example record
updates, comprehensions, and operator sections) is not implemented.

Rejected example:

```haskell
f x = (\y -> y) x
```

The earlier higher-kinded audit failures were generator defects, not scope
exclusions. Their regressions now pass. Structural list/datatype comparison
remains outside the strict-scalar primitive policy and is rejected explicitly.

## Tests

```sh
# Translator build, including the registered local runtime checkout:
stack build

# All generated-code acceptance tests, handwritten feasibility probes,
# and runtime regressions:
stack --stack-yaml integration/stack.yaml test

# One focused generated-code boundary:
stack --stack-yaml integration/stack.yaml test cost-translate \
  --test-arguments=scanl-unused-input

# Export diagnostics and the successful explicit-export example:
stack --stack-yaml integration/stack.yaml test cost-translate \
  --test-arguments='diagnostics explicit-constructor-exports'

# Higher-kinded translation, conversion and structural-demand regressions:
stack --stack-yaml integration/stack.yaml test cost-translate \
  --test-arguments='higher-kinded-datatype-field higher-kinded-recursive-field higher-kinded-polykinded-field higher-kinded-demands'

hlint src app test integration/probes integration/test-integration

# Standalone evidence for the structural GHC output builders:
stack --stack-yaml integration/stack.yaml exec -- \
  runghc -package=ghc -package=ghc-paths -isrc test/OutputAstProbe.hs
```

The full suite passes: 65 generated fixed-input value/cost examples, the generated
100-trial QuickCheck property, first-order and higher-kinded datatype/demand
checks, 17 CLI diagnostic cases and demanded-failure checks. An invalid output
module name is reported in its own generated-code category (invalid module
name) and retains the candidate text, rather than being relabeled as
unsupported source. Pattern-binding and local-signature errors include the
source location (and the pattern, where applicable). A separate public
compilation check treats unused
local bindings, redundant patterns and simplifiable class constraints as errors.
The higher-kinded demand
test includes 100 bounded infinite-structure trials. The ten-equation regression checks 169 observations and
limits generated size: shared failure actions reduced it from 2,030,630 bytes
to 12,085 bytes (11,732 after the AST migration before Char support added its
local instances). All 37 handwritten tests, all 9 scanl runtime
checks, and the polymorphic-sharing and higher-kinded runtime gates pass.
The obsolete bytecode-loading harness was retired
in favor of the real subprocess acceptance tests. HLint reports no hints.
Handwritten probe results are distinct from tests of generated code.

Tests retain temporary source files, generated modules and compiler/run logs for
diagnosis. No user cost properties or arbitrary source generators are translated.
