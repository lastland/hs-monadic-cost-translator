# Haskell Cost-Monadic Translator

`cost-translate` translates a single pure Haskell source module into an instrumented module targeting the Memorist cost-semantics runtime (`MemoristANT`). The generated module makes laziness and sharing explicit: datatype fields and locally shared bindings become named thunks (`Th`), computations run in the cost monad (`MemoristM`), and every source function call charges one tick. Alongside translated bindings, the translator generates thunked representation types, demand types, pure-to-monadic conversion instances, and QuickCheck demand generators for property-based cost testing.

## Prerequisites and Setup

Building and running the translator with candidate verification requires:

- **GHC 9.10.3**: The translator uses GHC 9.10.3's internal API. Cabal rejects other compiler versions during dependency resolution.
- **Stack (LTS 24.22)**: `stack.yaml` pins LTS 24.22 (GHC 9.10.3) and `QuickCheck-2.17.1.0` to match the target runtime.
- **Local `MemoristANT` checkout (`hs-pbt/`)**: Place a compatible `MemoristANT` checkout at `hs-pbt/` in the repository root and apply the [private-allocator patch](patches/README.md) before building (`hs-pbt/` is git-ignored and not bundled with this repository):
  ```sh
  git apply --check --directory=hs-pbt patches/memorist-private-allocator.patch
  git apply --directory=hs-pbt patches/memorist-private-allocator.patch
  stack build MemoristANT:lib
  ```

`stack.yaml` registers both `.` and `hs-pbt/` so that GHC commands invoked under `stack run` and `stack test` can load `MemoristANT` and `QuickCheck` when typechecking generated modules. The translator library and executable do not depend on `MemoristANT` at build time. Runtime-free builds require Cabal and GHC 9.10.3; repository validation also requires the pinned tools described in the [source-check guide](docs/source-policy.md).

## Building and Translating a Module

Build the translator executable:

```sh
stack build
```

Translate a pure Haskell source module to a target path:

```sh
stack run cost-translate -- SOURCE.hs OUTPUT.hs
```

`cost-translate` takes two arguments:

1. `SOURCE.hs`: Path to the input Haskell module.
2. `OUTPUT.hs`: Destination path for the generated module. The filename base must be a valid uppercase Haskell module name (for example, `Generated.hs` emits `module Generated where`).

Each run writes a companion log to `OUTPUT.hs.log` with binding arities, call-site classifications, and compiler diagnostics. Before publishing `OUTPUT.hs`, the translator writes the candidate to a temporary directory next to the output and typechecks it with an external `ghc -fno-code` process against `MemoristANT` and `QuickCheck`.

If typechecking succeeds, the translator atomically renames the candidate to `OUTPUT.hs`. If source validation or candidate typechecking fails, the translator records the diagnostic (and any rejected candidate) in `OUTPUT.hs.log`, exits with a non-zero status, and leaves any existing `OUTPUT.hs` untouched.

Run `make examples` from the repository root to translate every source in [`examples/`](examples/). The [examples guide](examples/README.md) includes GHCi walkthroughs with expected values and tick counts.

## Using Generated Code

Every translated source function is an ordinary module-level function that can be called directly without a module initializer or environment record. Top-level value bindings translate to monadic actions:

```haskell
seedM :: MemoristM Int
doubleM :: Th Int -> MemoristM Int
```

Each generated module imports its source module qualified as `Source` (`import qualified <SourceModule> as Source`) so conversion instances can map between pure source datatypes and thunked representations. When compiling client drivers or loading generated modules into GHCi, keep the source module's directory on GHC's search path (`-i<sourceDir>`) and expose `MemoristANT` and `QuickCheck`.

Client drivers check results and measure execution costs with combinators from `Memorist.Memorist`:

- `correct` compares a monadic result with an expected pure source value.
- `infcost` measures demanded ticks given a demand function such as `fullDemand`; `infcost'` accepts a concrete structural demand value instead.
- `fromPure1`, `fromPure2`, and related helpers convert pure arguments into thunked arguments for translated functions.

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

This prints `True` and `1`: `doubleM` produces the expected value and charges one entry tick.

Functions that reference top-level value bindings keep the same direct calling convention. For example, after translating [`examples/Shared.hs`](examples/Shared.hs):

```haskell
program :: MemoristM Int
program = do
    a <- fromPure1 firstM (5 :: Int)
    b <- fromPure1 secondM (5 :: Int)
    pure (a + b)
```

Because top-level values become monadic actions without a global cache, executing a global action multiple times recomputes it rather than sharing a single module-wide thunk; see [Limitations](#limitations) below.

## Cost Model, Laziness, and Local Sharing

### Entry-Tick Cost Model

The translator instruments renamed source syntax (via the GHC API) rather than optimized Core to preserve source-level call boundaries and call-by-need evaluation (see [`docs/adr/0001-source-cost-semantics.md`](docs/adr/0001-source-cost-semantics.md)):

- **1 tick per source function entry**: Every translated top-level or local function executes `tick` on entry.
- **1 tick per generated record selector and direct container fold**: Record field accessors (`<field>M`) charge one entry tick before forcing the record and selected field. Direct `FoldableT` operations (`foldrT` and `foldlT` on `MaybeT` and `PairT`, plus generic `concatMapT`) charge one entry tick in addition to callback ticks; list instances delegate to `Memorist.Example.List`.
- **0 entry ticks for constructors, adapters, and conversions**: Allocating data constructors, pairs, lists, or records suspends field computations without charging an entry tick. Partial application, function composition `(.)`, and representation or demand conversions (`reprM`, `abstract`, `fullDemand`) also add no entry ticks.
- **0 entry ticks for scalar primitives**: Arithmetic (`+`, `-`, `*`, `/`, `div`, `mod`, `quot`, `rem`), negation, scalar comparisons (`==`, `/=`, `<`, `<=`, `>`, `>=`), and short-circuit booleans (`&&`, `||`) charge no entry tick and force only the operands they need.

Unticked operations still pay the ticks of any thunks they force. Costs count source-level function entries, not elapsed wall-clock time or GHC-optimized execution.

### Laziness and Local Sharing

Translated bindings express evaluation and sharing through the runtime's core monadic and thunk primitives—`lazy`, `force`, `forcing`, `bind`, `ret`, and `tick` (rendered using standard Haskell `do` notation and `pure`)—together with metadata-preserving structural representation views:

- **Call-by-need suspension and reuse**: Function arguments, constructor fields, and local `let`/`where` value bindings are suspended with `lazy` and evaluated at most once per binding instance when demanded with `force`. Passing an existing thunk as an argument or field reuses it directly without allocating a wrapper thunk.
- **Lazy pattern matching and guards**: Variable and wildcard (`_`) patterns preserve the subject thunk's identity without forcing it; in an `as`-pattern (`x@pat`), the alias `x` preserves the subject's thunk identity while matching `pat` forces whatever layers `pat` inspects. Constructor and literal patterns force only the layer they inspect, and untaken equations, guards, or conditional branches neither allocate thunks nor charge ticks. Local bang bindings without class contexts (`let !x = rhs in body`) force the outer value before continuing while leaving constructor fields lazy and sharing `x`.
- **Local polymorphic and dictionary-sensitive sharing**: Each function invocation allocates fresh local thunks. For supported structural result and constructor-payload shapes, unconstrained polymorphic local values share a single native thunk across type instantiations through pure structural views that preserve thunk identities and annotations without forcing unused fields. Constrained local values in supported scopes and shapes share when the type arguments determining their dictionaries agree, allocating distinct thunks for distinct dictionary instantiations; remaining shapes fall back to the legacy helpers noted in [Limitations](#limitations).

## Supported Source Subset and Generated Support

- **Algebraic datatypes**: `data` declarations with multiple or nullary constructors, type parameters, recursion, nested non-regular recursion (e.g., `data Nested a = End a | More (Nested [a])`), higher-kinded parameters (`data Higher f a = Higher (f a)`), recursive constructor applications (`data Fix f = In (f (Fix f))`), infix constructors, and named record fields.
- **Patterns, guards, and expressions**: Ordered function equations and `case` alternatives using variable, wildcard, constructor, list (`[]`, `:`), boxed pair `(a, b)`, record, `as`, and literal (numeric, `Char`, `String`) patterns; boolean, pattern (`pat <- expr`), `let`, and comma-separated conjunctive guards; `let` and `where` clauses with local functions (including mutually recursive groups) and non-recursive named value bindings; `if-then-else`; `case-of`; `undefined`; literal-string `error "msg"`; and explicit type signatures and expression annotations.
- **Selected library operations**: `map`, `foldr`, `foldl`, `concatMap`, `filter`, `zipWith`, `scanl`, `head`, `tail`, and `(.)` from `Prelude`, plus `repeatly` from `Memorist.Example.NewtonRaphson`. Folds operate over lists, `Maybe`, and boxed pairs via `FoldableT`. Only these selected operations are replaced, and replacements follow resolved names rather than lexical shadowing.
- **Generated datatype and QuickCheck support**: For each source datatype `Tree a`, the generated module defines a thunked representation type (`TreeT`), a demand type (`TreeD` wrapping fields in `ThD` with `Skip` and `Need`), `Repr` and `Demand` instances (`reprM`, `abstract`, `fullDemand`), and budget-bounded QuickCheck demand generators and compatible shrinkers (`genTreeDemand`, `shrinkTreeDemand`).

## Limitations

### Excluded Source-Global Sharing Costs and Identity

Top-level value bindings translate to monadic actions, and translated functions take no global environment argument. Re-executing a global action does not retain its earlier result or thunk identity across uses:

- **Global sharing costs**: Memorist does not recover source-global sharing costs through direct top-level calls. For instance, the worked `work/shared/f/g/test` program charges 5 ticks at runtime rather than 4 in the source cost model. Global sharing costs are outside the translator's current scope ([issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2#issuecomment-6972277826)); the translator does not add a global cache or pre-evaluate globals to compensate.
- **Global thunk identity**: Repeated executions of a global action also allocate fresh thunks rather than preserving global thunk aliases across calls, even when measured tick counts happen to agree ([issue #4](https://github.com/lastland/hs-monadic-cost-translator/issues/4)). Local sharing, local thunk identities, and per-invocation freshness remain supported.

### Incomplete Six-Combinator Construction-Only Contract

The target construction-only contract restricts generated code to the six runtime combinators `lazy`, `force`, `forcing`, `bind`, `ret`, and `tick` (with `do`/`pure` notation) plus checked metadata-preserving structural views ([issue #7](https://github.com/lastland/hs-monadic-cost-translator/issues/7#issuecomment-7000190468)). Full compliance is incomplete:

- **Legacy local sharing fallback**: Local value shapes outside the native lowering rules still emit legacy evaluator helpers (`sharedAt` / `sharedAtBy` using `eval` and `value`) and may require `Typeable` constraints absent from the source signature. In particular, construction-only sharing for potentially overlapping generic instantiations—such as a dictionary-sensitive local used at both an abstract `a` and concrete `Int`, which must share when `a = Int` and stay separate when `a = Integer`—is outside the current scope ([issue #3](https://github.com/lastland/hs-monadic-cost-translator/issues/3)).
- **Generated conversion and demand helpers**: Generated `Repr`, `Demand`, and `GeneratedDemand` support instances inspect thunk payloads and accounting metadata directly (`tval`, `usage`, `empty`, `union`), which have no exception under the six-combinator contract.

### Unsupported Source Constructs and Semantic Constraints

- **Unsupported syntax**: Anonymous lambdas (`\x -> ...`) and operator sections; `newtype`, type synonyms, type families, GADTs, and existential or constrained data constructors; user-defined typeclasses, instances, and `deriving` clauses; strict field annotations (`!a`), strict patterns other than unconstrained local bang bindings, irrefutable patterns (`~pat`), and `Strict`, `StrictData`, or `RebindableSyntax`; pattern bindings (`let (a, b) = expr`) and recursive value bindings (`let xs = 1 : xs`); function aliases or computed functions lacking a static syntactic arity, and over-saturated applications; higher-rank user types, record updates (`r { field = x }`), list comprehensions, arithmetic sequences, and tuples other than boxed pairs `(a, b)` (and `()` as a scalar).
- **Scalar-only comparisons**: Runtime `Eq` and `Ord` instances on thunked types compare thunk metadata rather than structural values. The translator rejects `Eq` and `Ord` uses unless their operands are strict scalars (`Int`, `Integer`, `Float`, `Double`, `Bool`, `Char`, `()`) or `Scalar`-constrained type variables.
- **Restricted class constraints**: Typeclass constraints on translated bindings are limited to `Foldable` (mapped to `FoldableT`) and standard scalar/numeric classes (`Num`, `Eq`, `Ord`, `Fractional`, `Floating`, `Integral`, `Real`, `RealFrac`, `RealFloat`, `Enum`, `Bounded`).
- **Single-module boundary, imports, and exports**: The translator processes one module at a time and permits imports only from `Prelude` and `Memorist.Example.NewtonRaphson` (`repeatly`). Because the generated module imports the source module to define `reprM` and `abstract`, any source module with an explicit export list must export all of its datatypes and all of their constructors (e.g., `Box(..)`).

## Testing and Validation

Test groups and selectors are defined in [`test/Main.hs`](test/Main.hs).

- **Runtime-free build and policy checks** (requires Cabal, Python 3, Git, and the [pinned source-check tools](scripts/source-policy.json); does not require `hs-pbt/`):
  ```sh
  sh scripts/validate.sh
  ```
  `scripts/validate.sh` checks formatting, source limits, ShellCheck, HLint, and checker/hook regressions before running a warning-clean build and runtime-free tests. See [`docs/source-policy.md`](docs/source-policy.md) for tool setup, formatting commands, and the opt-in staged hook.
- **Full reference-comparison test suite** (requires Stack, `hs-pbt/`, and HLint on `PATH`):
  ```sh
  sh scripts/stack-test.sh
  ```
  Run individual test groups or fixture selectors with `--test-arguments`:
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
