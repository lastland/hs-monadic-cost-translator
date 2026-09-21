# Haskell cost-monadic translator

A proof-of-concept CLI translator from one pure Haskell module into a separate readable Haskell
module for the external Memorist runtime (the GHC package `MemoristANT`): explicit named thunks
(`Th`), the cost monad (`MemoristM`), one tick per source-function invocation, and structural
demand values for QuickCheck cost testing. The translator is built on the GHC API, does not
depend on the runtime, and typechecks every generated candidate with an external `ghc` before
publishing it.

## Installation and use

Prerequisites: Stack with the pinned resolver (**LTS 24.22 / GHC 9.10.3**, in `stack.yaml`),
and a locally supplied checkout of the external runtime at `hs-pbt/` (git-ignored; the
repository neither bundles nor downloads it). The root Stack project registers that checkout
and QuickCheck 2.17.1.0 (pinned as an extra-dep) so GHC's environment under `stack run` can
typecheck generated candidates; the checkout must exist locally for these commands.

```sh
stack build
stack run cost-translate -- SOURCE.hs OUTPUT.hs
```

The output basename must be an uppercase Haskell module name, such as `Generated.hs`.
Ready-to-try sources live in [`examples/`](examples/); [`examples/README.md`](examples/README.md)
has per-file walkthroughs with verified values and costs.

The tool writes `OUTPUT.hs.log`: source-located resolved function identities, syntactic
arities, supplied argument counts, adapters, and compiler diagnostics. A failed translation
retains its log. A failed candidate never replaces the output file, so an older successful
output can remain after a later failure — check the command's exit status and the log.
Diagnostics distinguish source syntax/type errors, unsupported source, missing target-package
setup, generated-code type errors, and an invalid output module name; the generated-code
categories indicate a translator defect or unhandled implementation case, not a successful
translation.

### Using the generated code

The output imports the source module (qualified as `Source`) for conversions, so both must
remain compilable together. Compile with `ghc` — invoked from `PATH`, so use the same pinned
compiler — exposing `MemoristANT` and `QuickCheck`. A handwritten driver checks the generated
value against the source and measures the demanded cost:

```haskell
correct (fromPure1 sharedM (5 :: Int)) (20 :: Int)

infcost (fromPure1 sharedM (5 :: Int)) fullDemand
-- Cost 2: one shared invocation, one double invocation
```

Costs are source-level costs: a demanded failure is still a failure, and no cost is promised
for it.

If the source has top-level value bindings, the output exports a `Bindings` record and
`initialize :: MemoristM Bindings`. Initialize once per evaluation and reuse its fields across
calls to keep top-level thunks shared:

```haskell
do bindings <- initialize
   a <- fromPure1 (firstM bindings) (5 :: Int)
   b <- fromPure1 (secondM bindings) (5 :: Int)
   pure (a + b)
```

### Running the tests

The integration project is a development/test environment, not a translator dependency, and
assumes the external runtime already exists:

```sh
stack --stack-yaml integration/stack.yaml test

# Focused cases:
stack --stack-yaml integration/stack.yaml test cost-translate --test-arguments=scanl-unused-input
stack --stack-yaml integration/stack.yaml test cost-translate --test-arguments='diagnostics explicit-constructor-exports'

hlint src app test integration/probes integration/test-integration
```

The runtime's `scanlM` must include the demand-laziness correction (delayed input-spine forcing
and delayed callback forcing); the runtime regression suites verify it. Tests deliberately
retain temporary fixtures and logs for diagnosis.

## Features

### Pipeline

GHC parses the source, resolves names, and infers types; translation instruments the renamed
syntax before optimization. Every emitter constructs GHC `GhcPs` syntax through
`Cost.Translate.Output`, and `Pretty` renders the completed module once — there is no reparse
of assembled source.

### Cost policy

Each translated source function invocation costs one tick, including trivial wrappers and local
helpers. Generated record selectors also charge one tick. Value bindings and constructor
construction do not tick. Scalar primitives retain the costs of their operands without adding
entry ticks. Composition and partial application do not tick, but invoked functions do.

Handwritten runtime replacements keep their own instrumentation. `concatMapM` delegates to
instrumented `foldrM` and `appendM`; it is not a single-tick primitive. `repeatlyM` has no
entry tick, so its producer cells do not add ticks; demanded callbacks retain their own ticks.
The corrected `scanlM` ticks once per demanded scan invocation, including the seed cell. Each
direct Maybe/pair fold method charges one entry tick, then retains the callback's ticks when
invoked; a Nothing fold therefore costs its entry tick without forcing the callback. The
generic `concatMapT` method charges one entry tick and uses direct `foldrT` plus the
handwritten `appendM`.

### Language subset

The end-to-end tests exercise:

- Inferred polymorphic function signatures and shared polymorphic values.
- Named recursive functions, mutually recursive local helpers, captured values, non-recursive
  `let`/`where` values, and top-level shared initialization.
- Ordered equations, constructor/list/literal/as/wildcard/variable patterns, `case`, Boolean
  guards, pattern guards and `let` guards. Literal patterns include numbers, characters and
  strings; a string literal pattern decomposes into its character sequence on the
  representation, forcing cell by cell and stopping at the first mismatch like the source
  comparison.
- Record fields are extracted by their declaration positions, but supplied record subpatterns
  are tested in source order. A mismatch can therefore skip a later failing field even when
  that field was declared earlier.
- Arithmetic and comparisons on scalar values, and short-circuit `&&`/`||`. `Char` is a
  scalar primitive throughout: fields, conversion, demands and `Eq`/`Ord` comparisons. The
  external runtime has no Char instances, so each generated module carries local identity
  conversions (`Rep Char = Char` and the demand counterparts) instead of changing the
  runtime.
- Suspended `undefined`, literal-message `error`, and demanded match failures.
- Named function arguments, captured local callbacks, composition, and direct partial
  application at multiple argument counts, including four-argument functions. Adapters do not
  add ticks.
- Prefix `(.)`, applied compositions and partial applications of compositions. Captured
  operands and intermediate thunks are shared across repeated uses of the resulting callback,
  including nested compositions. The right operand may have any positive arity: `(map . add)
  x` supplies the partially applied `add x` as the mapped callback, with `add` ticking once
  per demanded element and no tick for the partial application itself.
- Lazy algebraic datatype representations, records, selectors, infix constructors, lists,
  pairs and `Maybe`. Every ordinary field is thunked, including scalar fields.
- Nested recursive parameters such as `Nested [a]` and `Nested (a,a)`; there is no blanket
  regular-recursion restriction.
- Higher-kinded fields (`Higher f a`), recursive constructor parameters (`Fix f`), and
  inferred kind polymorphism.

### Library replacements

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

Only resolved imported identities select replacements; same-named source definitions are
translated normally. Implicit and explicit Prelude imports are supported for these operations
and the implemented primitives. The only additional admitted import module is
`Memorist.Example.NewtonRaphson`, for `repeatly` only; its other operations are not
automatically supported. There is no user-module dependency traversal. An inferred source
constraint `Foldable t` becomes `FoldableT t` over the translated representations; `foldrT` and
`foldlT` operate directly on lists, `MaybeT`, and partially applied `PairT`, without converting
Maybe and pairs to artificial lists. This is a fixed supported Prelude interface, not general
source typeclass translation; user-written classes and instances remain outside the subset.

### Datatypes and demands

For `Tree`, generation emits `TreeT`, `TreeD`, `Repr`, `Demand`, `LBoundDemand`, `DemandArb`
and the representation-demand marker instances; conversion instances
(`reprM`/`abstract`/`fullDemand`) refer to the original source module.

For an ordinary `Tree a`, the generated demand functions have the following normalized types
(the emitted signatures use the equivalent kind-aware `GeneratedK` constraint and `DemOf`
family):

```haskell
genTreeDemand    :: GeneratedDemand a => Int -> TreeT a -> Gen (TreeD (Dem a))
shrinkTreeDemand :: GeneratedDemand a => TreeT a -> TreeD (Dem a) -> [TreeD (Dem a)]
```

`arbDemandFrom` uses QuickCheck's size. The generator matches the observed constructor and
distributes a finite budget among fields: `Skip` on exhaustion, otherwise the default 1:3
Skip:Need weighting, and no skipped field is evaluated. Budgets conservatively charge for both
the demanded field and the observed structure; a finite budget bounds demand generation, not
the source program's time to satisfy that demand. Compatible shrinkers keep observed
constructors and either skip fields or recursively shrink already demanded fields. For
result-compatible testing, use the result-indexed functions rather than standalone `arbitrary`
demands: `forAllShrink (genTreeDemand 20 result) (shrinkTreeDemand result) property`. Custom
generators and shrinkers can be supplied directly to `forAllShrink`. Standalone demand
`Arbitrary` instances exist for the external class superclasses and have no default shrinks;
they are not a substitute. The translator generates representation and demand support, not user
cost specifications.

### Higher-kinded datatypes

The declarations `data Higher f a = Higher (f a)`, `data Fix f = In (f (Fix f))`, and `data
Mixed f a = Leaf a | Branch (f (Mixed f [a]))` need no translator-specific annotation. The
output generates kind-indexed representation/demand mappings: at kind `Type`, `RepOf` and
`DemOf` use the existing runtime `Rep` and `Dem` families; at constructor kinds, they map
lists, Maybe, pairs and the selected module's datatypes to their generated
representations/demands, including partial applications. Invisible kind arguments are
distinguished from visible datatype parameters; inferred kinds are retained in binders and
signatures. Literal and promoted indices are preserved at their index kinds.

Quantified application constraints lift the runtime interfaces through type application.
Explicit dictionary witnesses make the mapping equalities available inside recursive
conversion/demand methods. `GeneratedK` lifts the bounded structural generator interface;
`ShapeK` supplies Eq/Show/Arbitrary dictionaries for demand values. These are generated
adapters over the external interfaces, not new source typeclass support or changes to the
runtime API. `ReprK` and `DemandK` are constraint synonyms; the application-witness classes
remain where quantified constraint heads require them. Conversion adds no ticks, and every lazy
field remains a thunk.

### Top-level sharing

Unconstrained polymorphic values reserve one runtime name and retain sharing across type
instantiations. Dictionary-constrained values instead use a stable subtree per instantiation of
the generalized variables occurring in their constraints. Repeated uses of the same constrained
instantiation share; variables absent from the constraints do not split that sharing. For
example, `Num a => (a,[b])` shares across changes to `b` while distinguishing different `a`
dictionaries.

The generated code obtains the required `Typeable` evidence in affected value and caller
signatures. Keys structurally encode qualified type constructors, kind arguments and
applications, rather than hashing or printing short names. Reserved name delimiters isolate the
instance subtrees from ordinary allocations and from each other, including nested shared
bindings. GHC type evidence retains source annotations and defaulted instantiations; source
annotations are not required solely for translation.

### Paper terminology bridge

The generated code uses the runtime vocabulary from the semantics paper: the cost monad
`MemoristM`; named thunks `Th`, allocated with `lazy` and evaluated with `eval`; ticks as the
unit of cost; the `Rep`/`Dem` type families relating source and thunked representations; demand
values that structurally describe how far to force a value; and
`correct`/`infcost`/`fullDemand` for value and cost checks.

### Repository layout

| Path | Purpose |
| --- | --- |
| `app/Main.hs` | CLI entry point |
| `src/Cost/Translate.hs` | Orchestration: generate, log, typecheck, publish by rename |
| `src/Cost/Translate/Frontend.hs` | GHC session and whole-module translation |
| `src/Cost/Translate/Output.hs` | Structural `GhcPs` builders |
| `src/Cost/Translate/Pretty.hs` | Single rendering pass, sectioned output |
| `src/Cost/Translate/Names.hs` | Generated-name scheme |
| `src/Cost/Translate/Datatypes.hs` | Representation/demand types, conversions, generators |
| `src/Cost/Translate/Kinds.hs` | Kind-indexed mappings and liftings |
| `src/Cost/Translate/Sharing.hs` | Shared-thunk allocation, dictionary-keyed sharing |
| `src/Cost/Translate/Callable.hs` | Application/composition spines and adapters |
| `src/Cost/Translate/Foldable.hs` | Generated `FoldableT` |
| `src/Cost/Translate/Validation.hs` | Scalar comparison gate |
| `src/Cost/Translate/Typed.hs` | Inferred type evidence |
| `src/Cost/Translate/Diagnostic.hs` | Diagnostic categories |
| `docs/adr/0001`–`0004` | Architecture decision records: cost semantics, GHC API boundary, external runtime interface, explicit thunks |
| `examples/` | Ready-to-try source modules with verified values and costs |

## Limitations

### Excluded constructs

The translator rejects the following constructs; each fails whole-module translation with
source-located diagnostics: anonymous lambdas; function aliases or computed function results
without validated calling conventions; unrestricted higher-order source definitions; recursive
value bindings; lazy pattern bindings; explicit irrefutable patterns; strict fields/patterns;
`Strict`/`StrictData` (and `RebindableSyntax`); `seq`; strict application; `newtype`; source
classes, instances and deriving; higher-rank types; polymorphic recursion; multi-module
translation; record updates, comprehensions, operator sections and other unimplemented syntax;
existential or constrained constructors; and imports beyond `Prelude` and
`Memorist.Example.NewtonRaphson`. The rejected example `f x = (\y -> y) x` fails as an
anonymous lambda.

### Semantic constraints

The source module must remain available and compilable next to the output, because the output
imports it as `Source`. Generated conversions additionally require the source datatype and all
its constructors to be exported. With an explicit export list, write:

```haskell
module Example (Box(..), make, readBox) where
data Box = Box Int
make x = Box x
readBox (Box x) = x
```

`module Example (Box, make, readBox) where` hides the constructor. The translator rejects it at
the constructor's source location with the diagnostic `generated conversions require the source
module to export constructor Box`; partially exported constructor sets and wholly private
datatypes are detected the same way, before generating/typechecking a candidate. Source exports
are not modified.

The runtime's `Eq`/`Ord` instances on thunked structures compare thunk metadata, not source
structural equality. Primitive comparisons are therefore validated against a strict scalar
domain before generation, and generated polymorphic Eq/Ord signatures carry a `Scalar`
constraint. Desugared types are inspected for this validation only; all instrumentation uses
renamed syntax. Structural list/datatype comparison is rejected explicitly.

These are source-level costs, not GHC-optimized execution costs. A demanded failure remains a
failure, and no cost result is promised for a failing computation.

### Environment constraints

Translation stops at the single-module boundary: there is no user-module dependency traversal.
The CLI installs nothing and accepts no custom runtime path. Generated code requires a GHC
environment already exposing `MemoristANT` and `QuickCheck`.

## AI usage declaration

This repository's code, tests, and documentation were developed with assistance from generative
AI tools within the OpenCode agent environment, under human direction and review. All code and
documentation were compiled and verified against the repository's automated test suites.
