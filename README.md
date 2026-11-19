# Haskell Cost-Monadic Translator

`cost-translate` translates a single pure Haskell source module into an instrumented module targeting the Memorist cost-semantics runtime (`MemoristANT`). The generated module makes sharing and evaluation explicit: datatype fields and shared bindings become named thunks (`Th`), computations run in the cost monad (`MemoristM`), and every source function call charges one tick. Alongside translated bindings, the translator generates thunked representation types, demand types, pure-to-monadic conversion instances, and QuickCheck demand generators for property-based cost testing.

## Prerequisites and Setup

Building and running the translator with full candidate verification requires:

- **GHC 9.10.3**: This is the only supported compiler version. Cabal rejects other versions during dependency resolution.
- **Stack (LTS 24.22)**: `stack.yaml` pins LTS 24.22 (GHC 9.10.3) and `QuickCheck-2.17.1.0` to match the target runtime.
- **Local `MemoristANT` checkout (`hs-pbt/`)**: Place a local checkout of the `MemoristANT` package at `hs-pbt/` in the repository root. Apply the [private-allocator patch](patches/README.md) before building. This directory is git-ignored and not bundled with the repository.

`stack.yaml` registers both `.` and `hs-pbt/` so that GHC commands run under `stack run` and `stack test` can load `MemoristANT` and `QuickCheck` when typechecking generated modules. The translator library and executable themselves do not depend on `MemoristANT` at build time. Runtime-free builds need Cabal and an installed GHC 9.10.3; repository validation also requires the pinned tools described in the [source-check guide](docs/source-policy.md).

## Building and Translating a Module

Build the translator executable:

```sh
stack build
```

Translate a pure Haskell source module to a target path:

```sh
stack run cost-translate -- SOURCE.hs OUTPUT.hs
```

`cost-translate` takes exactly two arguments:

1. `SOURCE.hs`: Path to the single input Haskell module.
2. `OUTPUT.hs`: Path where the generated module will be written. The output filename base must be a valid uppercase Haskell module name (for example, `Generated.hs` produces `module Generated where`).

A translation writes a companion log to `OUTPUT.hs.log` containing binding and call-site information, adapter choices, and compiler diagnostics.

Before publishing `OUTPUT.hs`, the translator typechecks the generated candidate with an external GHC process against `MemoristANT` and `QuickCheck`, using a temporary directory next to the output.

If typechecking succeeds, the translator atomically renames the candidate to `OUTPUT.hs`. If source validation or candidate typechecking fails, the translator records the diagnostic, exits with a non-zero status, and leaves any existing `OUTPUT.hs` untouched. Typechecking failures also include the rejected candidate in the log.

Runnable example modules and GHCi walkthroughs with expected values and tick counts live in [`examples/`](examples/) and [`examples/README.md`](examples/README.md).

## Using Generated Code

Each generated module imports its original source module qualified as `Source` (`import qualified <SourceModule> as Source`) so conversion instances can translate between source datatypes and thunked representations. When compiling client programs or loading generated modules into GHCi, keep the source module's directory on GHC's search path (`-i<sourceDir>`) and expose `MemoristANT` and `QuickCheck`.

Client drivers check value equivalence and measure execution ticks with combinators from `Memorist.Memorist`:

- `correct` compares the computed result with an expected pure source value.
- `infcost` measures ticks under a demand function. Passing `fullDemand` demands the whole result; `infcost'` accepts a concrete structural demand instead.
- `fromPure1`, `fromPure2`, and related helpers adapt pure arguments to translated functions.

For example, translate [`examples/Basic.hs`](examples/Basic.hs):

```sh
stack run cost-translate -- examples/Basic.hs Generated.hs
```

Save this driver as `Main.hs`:

```haskell
module Main where

import Generated
import qualified Basic as Source
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT

main :: IO ()
main = do
    -- Compare the monadic result against the pure expected value
    print (correct (fromPure1 doubleM (5 :: Int)) (Source.double 5))

    -- Measure exact ticks under full demand
    let cost = infcost (fromPure1 doubleM (5 :: Int)) fullDemand
    print (MT.toInteger cost)
```

Compile and run it in Stack's package environment:

```sh
stack exec -- ghc -iexamples -package MemoristANT -package QuickCheck Main.hs -o cost-example
./cost-example
```

It prints `True` and `1`: `doubleM` produces the expected result and charges one tick.

### Modules with Top-Level Value Bindings

Every translated source function is an ordinary module-level function. Top-level
values become monadic actions, including values used by several functions:

```haskell
seedM :: MemoristM Int
firstM :: Th Int -> MemoristM Int
```

No initializer or module environment argument is required. For example, after
translating [`examples/Shared.hs`](examples/Shared.hs):

```haskell
program :: MemoristM Int
program = do
    a <- fromPure1 firstM (5 :: Int)
    b <- fromPure1 secondM (5 :: Int)
    pure (a + b)
```

Executing a global action again does not retain its earlier result. Memorist
therefore does not currently recover source-global sharing costs through this
interface: the worked `work/shared/f/g/test` example costs 5 rather than the
source-model 4. Global sharing costs are outside the translator's current scope,
as specified in
[issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2#issuecomment-6972277826).
The source cost model is unchanged; the translator does not introduce a separate
global cache or evaluate computations to compensate for the discrepancy.

Repeated global actions can also lose thunk aliases across direct exported calls,
even when their measured costs agree. Global thunk identity is part of the same
runtime limitation in #2, as specified in
[issue #4](https://github.com/lastland/hs-monadic-cost-translator/issues/4).
Local sharing, local identities, and invocation freshness remain supported
requirements, not part of this deferral.

## Cost Model

The translator enforces source-level call-by-need cost semantics (see [`docs/adr/0001-source-cost-semantics.md`](docs/adr/0001-source-cost-semantics.md)):

- **1 tick per source function entry**: Every translated top-level or local function executes `M.tick` on entry.
- **1 tick per generated record selector**: Record field accessors translate to single-argument functions (`<field>M`) that charge one entry tick before forcing the record and selected field.
- **1 entry tick for direct `FoldableT` operations**: Direct container folds (`foldrT` and `foldlT` on `MaybeT` and `PairT`, plus generic `concatMapT`) charge one entry tick in addition to any ticks charged by their callback; list instances delegate directly to `Memorist.Example.List`.
- **0 ticks for constructor allocation**: Applying data constructors, tuple constructors, list literals, or record constructors allocates thunked fields in `MemoristM` without charging an entry tick.
- **0 entry ticks for scalar primitives**: Arithmetic (`+`, `-`, `*`, `/`, `div`, `mod`, `quot`, `rem`), negation (`-x`), comparisons (`==`, `/=`, `<`, `<=`, `>`, `>=`), and short-circuit booleans (`&&`, `||`) charge no entry tick; they force only the operands they need.
- **0 ticks for partial application and composition adapters**: Partially applying functions or composing callables with `(.)` builds unticked adapters; ticks occur only when the underlying functions are saturated and entered.
- **0 ticks for representation and demand conversions**: `reprM`, `abstract`, and `fullDemand` convert structures without charging ticks.

Unticked operations can still force computations that charge ticks. These costs model source-level work, not elapsed execution time or optimized GHC performance.

## Supported Language Subset and Generated Support

### Declarations, Patterns, Guards, and Expressions

- **Algebraic datatypes**: `data` declarations with multiple or nullary constructors, type parameters, recursion, nested non-regular recursion (e.g., `data Nested a = End a | More (Nested [a])`), higher-kinded parameters (`data Higher f a = Higher (f a)`), recursive constructor applications (`data Fix f = In (f (Fix f))`), infix constructors, and named record fields.
- **Pattern matching**: Ordered function equations and `case` alternatives using variable, wildcard (`_`), constructor, list (`[]`, `:`), boxed pair `(a, b)`, record, and as-patterns (`x@pat`), plus numeric, `Char`, and `String` literal patterns.
- **Guards**: Boolean guards, pattern guards (`pat <- expr`), `let` guards (`let x = expr`), and comma-separated conjunctive guards. Untaken branches neither allocate thunks nor charge ticks.
- **Local bindings and control flow**: `let` and `where` clauses containing local functions (including mutually recursive function groups) and non-recursive named value bindings; `if-then-else`; `case-of`; `undefined`; literal-string `error "msg"`; and explicit type signatures and expression type annotations.

### Library Replacements

Supported library operations include `map`, `foldr`, `foldl`, `concatMap`, `filter`, `zipWith`, `scanl`, `head`, `tail`, and composition `(.)` from `Prelude`, plus `repeatly` from `Memorist.Example.NewtonRaphson`. They use instrumented runtime implementations or generated adapters. Folds support lists, `Maybe`, and pairs through the generated `FoldableT` interface.

These are selected operations, not support for all of Prelude. Replacements follow resolved names: a local function named `map`, for example, is translated normally rather than replaced.

### Datatypes, Higher-Kinded Types, and Demand Generators

For each source datatype `Tree a`, the generated module emits:

- **Representation types** (`TreeT`): Constructor fields hold named thunks (`Th`).
- **Demand types** (`TreeD`): Fields use demand thunks (`ThD`), with `Skip` and `Need` describing which parts to evaluate.
- **Conversion and demand instances**: `reprM` converts pure source values to representations, `abstract` converts them back, and `fullDemand` demands the whole representation. Generated support also handles higher-kinded datatype parameters.
- **QuickCheck generators and shrinkers**: `genTreeDemand budget result` generates a finite demand compatible with the observed result. `shrinkTreeDemand result demand` reduces it while preserving compatibility. Skipped fields remain unforced.

### Call-by-Need Thunk Sharing

Local shared bindings evaluate on demand and reuse their thunks. Passing an existing thunk as an argument or field does not create a forwarding thunk. Variable and wildcard patterns do not force their subject; constructor and literal patterns inspect it only when reached.

Unconstrained polymorphic local values share across type instantiations.
Constrained local values share when the types determining their dictionaries
agree; other unconstrained type parameters do not split sharing. Top-level
actions use the dictionaries selected at each use but do not retain a shared
result across uses.

For unconstrained local values, one native allocation supports generalized
`Type` variables under lists, builtin `Maybe`, boxed pairs, and structurally
covariant source datatypes in any nesting. A bare type-variable result is also
supported. Parameters can vary independently, while fixed or captured fields
retain their types and original thunks. The translator allocates the complete
ordinary producer action once at an empty type instantiation. Pure structural
views preserve root and field thunk identities and annotations without forcing
unused payloads. Each function invocation still allocates fresh local thunks.

This result-directed path does not restrict the producer's expression form:
ordinary calls, local helpers, guards, cases, and recursive producers use the same
allocation. Datatype maps have explicit polymorphic signatures and a finite
parameter-dependency graph, including nonregular recursion such as
`data Nest a = End (Maybe a) | More (Nest [a])`. Only used maps and their
dependencies are emitted. A field such as `f a`, with an abstract captured `f`,
does not establish a structural map for `a`; no `Functor f` constraint is added.
Generalized higher-kinded and non-`Type` parameters remain outside this path.

These views refine the existing metadata-preserving structural conversion
mechanism. Its construction-interface policy remains tracked in
[issue #7](https://github.com/lastland/hs-monadic-cost-translator/issues/7);
arbitrary pure converters are not part of this interface.

If none of an unconstrained binding's quantified variables occurs in its result
type, the binding reuses one ordinary thunk without a representation view.
The variables may have higher or dependent kinds; their checked scopes remain
available to type annotations in the right-hand side.

When result-directed mapping does not apply, an unconstrained local value with a
translated source-datatype result can also
share one polymorphic thunk when its right-hand side is an unguarded,
nonrecursive constructor application or an `if` selecting such expressions.
Its generalized variables must have kind `Type` or the closed kind
`Type -> Type`, and fields depending on those variables must themselves meet
these same conditions. Explicit local type variables retain their checked kinds
within the right-hand side. Independent fields remain
suspended normally. Conditions run only when their enclosing payload is demanded;
the selected branch's fields remain lazy. The translator constructs polymorphic
payloads before suspending them
and uses pure typed views that preserve thunk identities and annotations.
This path needs no extra caller constraints or runtime operations. It does not
cover arbitrary polymorphic applications, recursive bindings, dependent kinds,
or other higher arrow kinds.

Within these payload expressions, saturated calls to module-level source
functions of type `forall a. a -> a` are also supported. The function must have
exactly one quantified `Type` parameter, an unrestricted arrow, and no class
context. Its argument must itself meet the payload-expression conditions above.
The translator suspends that argument at the polymorphic payload type and calls
the ordinary translated function, retaining its entry tick and lazy argument.
Local functions and other application types do not qualify for this path.

Dictionary-sensitive locals also use native allocation when they have one
generalized type parameter determining their dictionaries and every reference
has unambiguous type evidence. When that parameter is the whole result, closed
scalar references receive one thunk per type and source binding. References all
using one abstract type also share a native thunk when that type is available in
the declaration's lexical scope, including explicitly typed and
inferred helpers. Lists, `Maybe`, boxed pairs, and translated source datatypes can
wrap that same generalized parameter, as in `Maybe [a]`, `Maybe (a, a)`, or
`Maybe (Pair a b)` for a source-declared `Pair`. Matching uses the checked type
constructor identities and retains every type argument. Fixed closed fields are
also supported, as in `Maybe (a, Int)`, and must retain the same type at every use.
A field may instead be a captured type variable, as in `Maybe (a, b)`, when its
checked binder is available where the value is declared. That field must retain
the same lexical binder at every use; capturing it adds no caller constraints.
Within each use, all occurrences of the generalized parameter must agree on one
closed scalar type or one abstract type available in the declaration's enclosing
lexical scope. Native allocation preserves root and field identities and source
laziness at each level, including separately computed tuple fields with equal
right-hand sides. Allocation stays where the source value is declared: each
helper invocation creates fresh helper-local thunks. These bindings need no
runtime type keys or added reflection constraints.
Captured argument thunks keep their existing identities, including when callers
reuse them between invocations.

Native allocation also supports two or more generalized `Type` parameters when
all determine dictionaries, are recovered from the result type, and follow the
same container matching rules. Each parameter must instantiate to a closed
scalar or a type variable available in the declaration's lexical scope.
Scoped uses also need checked evidence for every required class
constraint at the declaration. That evidence can include superclass and
closed-instance facts from the enclosing binding. These checks also retain
GHC's solved dictionary facts for inferred bindings and closed dictionary facts
shared across the module. An enclosing binding's facts remain available to its
nested bindings; a sibling or descendant's local constraints do not become
available outside that binding. Within a constrained value's
right-hand side, nested declarations can use that value's quantified type
variables and class context. Those binders do not extend to sibling declarations
or to the enclosing value's own allocation.
When distinct instantiations involve scoped type variables, GHC must prove
that no pair can unify. For example, `(a, Int)` and `(a, Integer)` under
`Integral a` cannot unify, whereas `(a, Int)` and `(b, Int)` can overlap and
remain excluded under
[issue #3](https://github.com/lastland/hs-monadic-cost-translator/issues/3).
The translator allocates one ordinary `M.lazy` thunk per distinct admitted
result type and reuses it across repeated uses at that complete type; changing
any parameter selects a distinct thunk, even when only fields of the unchanged
parameters are demanded. Right-hand-side captures keep their enclosing scope,
and caller constraints remain unchanged without `Typeable` or new runtime
support. Unconstrained parameters do not qualify for this multiple-parameter
path. A type or dictionary available only inside a nested helper cannot justify
an allocation outside that helper.

The translator validates the full source before omitting unused local bindings.
References inside an unreachable local helper do not trigger thunk allocation or
reflection obligations in its enclosing function. Reachable local bindings retain
their source scope. Module-level functions and values remain in the output, even
when they are unused or unexported.

Local bang bindings without their own class contexts, such as
`let !x = work input in body`, demand the outer value before continuing. They
leave constructor fields lazy and retain the binding's shared thunk. Bindings
with a local class context retain the existing dictionary-sensitive limitations.

For a local helper with one generalized dictionary parameter, the
translator can specialize its generated type when every reference is a checked
prefix application, saturated infix call, or bare function reference at the same
verified instantiation.
The instantiation must use a closed scalar or a checked type variable available
where the helper is declared. A captured outer binding stays outside the helper:
repeated helper calls share that binding, while each helper call retains its
entry tick.
Prefix applications may supply fewer arguments than the helper accepts, but must
supply at least one. A bare reference supplies no arguments and needs the same
complete checked function type as every other reference. Creating either a bare
function value or a partial application is unticked; the helper charges its
entry tick only when its fully applied computation runs.
Parenthesized named callees retain their checked instantiation when one complete,
unambiguous parenthesis chain refers to the same function. The translator uses
that checked type rather than guessing an instantiation from the arguments.
Uninstantiated function values, sections, multiple instantiations, and escaping
type variables do not qualify for this helper specialization. A helper also stays
polymorphic when its specialized type cannot be named safely in its local scopes.
Top-level functions keep their generic interfaces.

A self-recursive helper can use this path when its recursive binding group
contains only that function. Real lexical self-references must have the helper's
complete checked body type, and external references must agree on one checked
instantiation. Each recursive entry still charges a tick; a shared value declared
outside the helper stays outside every recursive call. Mutual recursion and
self-calls at different types do not qualify for this path.

Local values outside these native rules can still emit legacy evaluator helpers
or require additional `Typeable` constraints. Those outputs fail the
construction-only contract enforced by the test suite. The overlapping-instantiation
limitation below is distinct from other scopes and result shapes outside the
native lowering rules.

## Architecture Overview

The translator uses the GHC API to parse, rename, and typecheck source modules. It instruments renamed source syntax rather than optimized Core, preserving source-level call boundaries. It inspects typed and desugared code for type evidence and validation, builds Haskell syntax trees, and pretty-prints the result before the external typechecking step.

See [`src/Cost/Translate.hs`](src/Cost/Translate.hs) for the translation entry point and [`docs/adr/`](docs/adr/) for design rationale.

## Limitations

### Unsupported Language Constructs

Unsupported constructs cause translation to fail rather than being silently omitted. They include:

- Anonymous lambdas (`\x -> ...`) and operator sections.
- `newtype` declarations, type synonyms, type families, GADTs, and existential or constrained data constructors.
- User-defined typeclasses, instances, and `deriving` clauses on source `data` declarations.
- Strict field annotations (`!a`), strict patterns other than the local bang bindings described above, irrefutable patterns (`~pat`), and `Strict`, `StrictData`, or `RebindableSyntax` language pragmas.
- Pattern bindings (such as `let (a, b) = expr`) and recursive value bindings (such as `let xs = 1 : xs`); only named function bindings and non-recursive named value bindings are supported.
- Function-valued aliases or computed function results that lack a static syntactic arity, and over-saturated function applications.
- Higher-rank user types, record updates (`r { field = x }`), list comprehensions, arithmetic sequences, and tuples other than boxed pairs `(a, b)` (and `()` as a scalar type).

### Semantic Constraints

- **Scalar-only comparisons**: Runtime `Eq` and `Ord` instances on thunked structures compare thunk metadata rather than structural values. The translator rejects `Eq` and `Ord` uses unless their operands are strict scalars (`Int`, `Integer`, `Float`, `Double`, `Bool`, `Char`, `()`) or `Scalar`-constrained type variables.
- **Explicit datatype and constructor exports**: Because the generated module is a separate Haskell module that imports the source module to define `reprM` and `abstract`, any source module with an explicit export list must export all of its datatypes and all of their constructors (e.g., `Box(..)`).
- **Restricted class constraints**: Typeclass constraints on translated bindings are limited to `Foldable` (translated to `FoldableT`) and standard scalar/numeric classes (`Num`, `Eq`, `Ord`, `Fractional`, `Floating`, `Integral`, `Real`, `RealFrac`, `RealFloat`, `Enum`, `Bounded`).

### Potentially Overlapping Generic Locals

Construction-only sharing for potentially overlapping generic instantiations is
outside the current scope, as tracked in
[issue #3](https://github.com/lastland/hs-monadic-cost-translator/issues/3).
For example, one dictionary-sensitive local may be used at both an abstract `a`
and concrete `Int`. Those uses must reuse one thunk when the caller chooses
`a = Int`, but remain distinct when it chooses `a = Integer`. Separate abstract
parameters can overlap in the same way.

These cases can still publish legacy helpers using `eval`/`value` and require
`Typeable` constraints absent from the source interface. They are not supported
construction-only translations; the global test guard still rejects them.
This exclusion does not apply to distinct closed instantiations or to other
scopes and result shapes merely because they currently use the same fallback.

### Module and Environment Constraints

- **Single-module boundary and restricted imports**: The translator processes one source file at a time and permits imports only from `Prelude` and `Memorist.Example.NewtonRaphson` (for `repeatly`).
- **External runtime environment**: Candidate verification and client compilation require GHC 9.10.3 with `MemoristANT` and `QuickCheck` installed in the package environment (configured via `hs-pbt/` in `stack.yaml`).

## Testing and Validation

Test groups and their execution are defined in [`test/Main.hs`](test/Main.hs).

- **Runtime-free build and policy checks** (requires Cabal, Python 3, Git, and the [pinned source-check tools](scripts/source-policy.json); does not require `hs-pbt/`):
  ```sh
  sh scripts/validate.sh
  ```
  `scripts/validate.sh` checks formatting, source limits, ShellCheck, HLint, and checker/hook regressions before the warning-clean build and runtime-free tests. See the [source-check guide](docs/source-policy.md) for setup, formatting commands, and the opt-in staged hook.
- **Full reference-comparison test suite** (requires Stack, `hs-pbt/`, and HLint on `PATH`):
  ```sh
  sh scripts/stack-test.sh
  ```
  To run individual test groups or fixture selectors:
  ```sh
  sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments="double"
  sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments="diagnostics fixture-policy"
  ```
- **Supplemental external-runtime probes** (validates `MemoristANT` runtime behavior directly):
  ```sh
  sh scripts/stack-test.sh --stack-yaml integration/stack.yaml
  ```
- **Linting**:
  ```sh
  sh scripts/check-hlint.sh
  sh scripts/check-shell.sh
  ```

## License

This project is licensed under the [MIT License](LICENSE). The external `hs-pbt/` checkout remains subject to its own license.

## AI Usage Declaration

This project uses generative AI tools under human direction and review.
