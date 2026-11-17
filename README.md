# Haskell Cost-Monadic Translator

`cost-translate` translates a single pure Haskell source module into an instrumented module targeting the Memorist cost-semantics runtime (`MemoristANT`). The generated module makes sharing and evaluation explicit: datatype fields and shared bindings become named thunks (`Th`), computations run in the cost monad (`MemoristM`), and every source function call charges one tick. Alongside translated bindings, the translator generates thunked representation types, demand types, pure-to-monadic conversion instances, and QuickCheck demand generators for property-based cost testing.

## Prerequisites and Setup

Building and running the translator with full candidate verification requires:

- **GHC 9.10.3**: This is the only supported compiler version. Cabal rejects other versions during dependency resolution.
- **Stack (LTS 24.22)**: `stack.yaml` pins LTS 24.22 (GHC 9.10.3) and `QuickCheck-2.17.1.0` to match the target runtime.
- **Local `MemoristANT` checkout (`hs-pbt/`)**: Place a local checkout of the `MemoristANT` package at `hs-pbt/` in the repository root. Apply the [private-allocator patch](patches/README.md) before building. This directory is git-ignored and not bundled with the repository.

`stack.yaml` registers both `.` and `hs-pbt/` so that GHC commands run under `stack run` and `stack test` can load `MemoristANT` and `QuickCheck` when typechecking generated modules. The translator library and executable themselves do not depend on `MemoristANT` at build time; runtime-free builds and policy checks need only Cabal and an installed GHC 9.10.3.

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
source-model 4. This runtime limitation is tracked in
[issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2).
The source cost model is unchanged; the translator does not introduce a separate
global cache or evaluate computations to compensate for the discrepancy.

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

For unconstrained local values whose generalized result is a type variable or
nested lists, the translator uses one native allocation. Pure representation
conversions retain the root and field thunk identities across type
instantiations. Each function invocation still allocates fresh local thunks.

Dictionary-sensitive locals also use native allocation when they have one
generalized type parameter determining their dictionaries and every reference
has unambiguous type evidence. When that parameter is the whole result, closed
scalar references receive one thunk per type and source binding. References all
using one abstract type also share a native thunk when that type is available in
the declaration's enclosing function scope, including explicitly typed and
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
function scope. Native allocation preserves root and field identities and source
laziness at each level, including separately computed tuple fields with equal
right-hand sides. Allocation stays where the source value is declared: each
helper invocation creates fresh helper-local thunks. These bindings need no
runtime type keys or added reflection constraints.
Captured argument thunks keep their existing identities, including when callers
reuse them between invocations.

For a nonrecursive local helper with one generalized dictionary parameter, the
translator can specialize its generated type when every reference is a checked
prefix application or saturated infix call at the same verified instantiation.
The instantiation must use a closed scalar or a checked type variable available
where the helper is declared. A captured outer binding stays outside the helper:
repeated helper calls share that binding, while each helper call retains its
entry tick.
Prefix applications may supply fewer arguments than the helper accepts, but must
supply at least one. Creating the partial application is unticked; the helper
charges its entry tick only when its fully applied computation runs.
Parenthesized named callees retain their checked instantiation when one complete,
unambiguous parenthesis chain refers to the same function. The translator uses
that checked type rather than guessing an instantiation from the arguments.
Bare function values, sections, multiple instantiations, and escaping type
variables do not qualify for this helper specialization. A helper also stays
polymorphic when its specialized type cannot be named safely in its local scopes.
Top-level functions keep their generic interfaces.

Potentially overlapping instantiations such as `a` and `Int`, unsupported scopes,
and other generalized result shapes still emit legacy evaluator helpers; those
outputs do not yet meet the construction-only contract enforced by the test
suite. The remaining polymorphic-local limitation is tracked in
[issue #3](https://github.com/lastland/hs-monadic-cost-translator/issues/3).

## Architecture Overview

The translator uses the GHC API to parse, rename, and typecheck source modules. It instruments renamed source syntax rather than optimized Core, preserving source-level call boundaries. It inspects typed and desugared code for type evidence and validation, builds Haskell syntax trees, and pretty-prints the result before the external typechecking step.

See [`src/Cost/Translate.hs`](src/Cost/Translate.hs) for the translation entry point and [`docs/adr/`](docs/adr/) for design rationale.

## Limitations

### Unsupported Language Constructs

Unsupported constructs cause translation to fail rather than being silently omitted. They include:

- Anonymous lambdas (`\x -> ...`) and operator sections.
- `newtype` declarations, type synonyms, type families, GADTs, and existential or constrained data constructors.
- User-defined typeclasses, instances, and `deriving` clauses on source `data` declarations.
- Strict field annotations (`!a`), strict or irrefutable patterns (`!pat`, `~pat`), and `Strict`, `StrictData`, or `RebindableSyntax` language pragmas.
- Pattern bindings (such as `let (a, b) = expr`) and recursive value bindings (such as `let xs = 1 : xs`); only named function bindings and non-recursive named value bindings are supported.
- Function-valued aliases or computed function results that lack a static syntactic arity, and over-saturated function applications.
- Higher-rank user types, record updates (`r { field = x }`), list comprehensions, arithmetic sequences, and tuples other than boxed pairs `(a, b)` (and `()` as a scalar type).

### Semantic Constraints

- **Scalar-only comparisons**: Runtime `Eq` and `Ord` instances on thunked structures compare thunk metadata rather than structural values. The translator rejects `Eq` and `Ord` uses unless their operands are strict scalars (`Int`, `Integer`, `Float`, `Double`, `Bool`, `Char`, `()`) or `Scalar`-constrained type variables.
- **Explicit datatype and constructor exports**: Because the generated module is a separate Haskell module that imports the source module to define `reprM` and `abstract`, any source module with an explicit export list must export all of its datatypes and all of their constructors (e.g., `Box(..)`).
- **Restricted class constraints**: Typeclass constraints on translated bindings are limited to `Foldable` (translated to `FoldableT`) and standard scalar/numeric classes (`Num`, `Eq`, `Ord`, `Fractional`, `Floating`, `Integral`, `Real`, `RealFrac`, `RealFloat`, `Enum`, `Bounded`).

### Module and Environment Constraints

- **Single-module boundary and restricted imports**: The translator processes one source file at a time and permits imports only from `Prelude` and `Memorist.Example.NewtonRaphson` (for `repeatly`).
- **External runtime environment**: Candidate verification and client compilation require GHC 9.10.3 with `MemoristANT` and `QuickCheck` installed in the package environment (configured via `hs-pbt/` in `stack.yaml`).

## Testing and Validation

See [`docs/testing.md`](docs/testing.md) for the reference-comparison architecture, probe catalog API, fixture layout, and static policy rules.

- **Runtime-free build and policy checks** (requires Cabal and GHC 9.10.3; does not require `hs-pbt/`):
  ```sh
  sh scripts/validate.sh
  ```
  `scripts/validate.sh` locates an installed `ghc-9.10.3` (or overrides via `GHC=/path/to/ghc`), builds the executable and test suite with `-Wall -Werror`, and runs the runtime-free diagnostic, selector, and fixture-policy checks.
- **Full reference-comparison test suite** (requires Stack, `hs-pbt/`, and HLint on `PATH`):
  ```sh
  stack test
  ```
  To run individual test groups or fixture selectors:
  ```sh
  stack test cost-translate:test:cost-translate-test --test-arguments="double"
  stack test cost-translate:test:cost-translate-test --test-arguments="diagnostics fixture-policy"
  ```
- **Supplemental external-runtime probes** (validates `MemoristANT` runtime behavior directly):
  ```sh
  stack --stack-yaml integration/stack.yaml test
  ```
- **Linting**:
  ```sh
  hlint src app test/*.hs test/reference/*.hs
  ```

## License

This project is licensed under the [MIT License](LICENSE). The external `hs-pbt/` checkout remains subject to its own license.

## AI Usage Declaration

This project uses generative AI tools under human direction and review.
