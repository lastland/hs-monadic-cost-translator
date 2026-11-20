# Haskell Cost-Monadic Translator

`cost-translate` translates a single pure Haskell source module into an instrumented module targeting the Memorist cost-semantics runtime (`MemoristANT`). The generated module makes laziness and sharing explicit: datatype fields and locally shared bindings become named thunks (`Th`), computations run in the cost monad (`MemoristM`), and every source function call charges one tick. Alongside translated bindings, the translator generates thunked representation types, demand types, pure-to-monadic conversion instances, and QuickCheck demand generators for property-based cost testing.

## Prerequisites and Setup

Building and running the translator with candidate verification requires:

- **GHC 9.10.3**: The translator uses GHC 9.10.3's internal API. Cabal rejects other compiler versions during dependency resolution.
- **Stack (LTS 24.22)**: `stack.yaml` pins LTS 24.22 (GHC 9.10.3) and `QuickCheck-2.17.1.0` to match the target runtime.
- **Local `MemoristANT` checkout (`hs-pbt/`)**: Place a local `MemoristANT-0.1.0.0` checkout at `hs-pbt/` in the repository root and apply the [private-allocator patch](patches/README.md) before building (`hs-pbt/` is git-ignored and not bundled with this repository, and no upstream repository URL or base commit revision is recorded):
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

Each source in [`examples/`](examples/) has a checked-in `*M.hs` translation.
Run `make` from `examples/` to refresh them. The [examples guide](examples/README.md)
includes GHCi walkthroughs with expected values and tick counts.

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
- **0 entry ticks for scalar primitives**: Arithmetic (`+`, `-`, `*`, `/`, `div`, `mod`, `quot`, `rem`), negation, scalar comparisons (`==`, `/=`, `<`, `<=`, `>`, `>=`), and short-circuit booleans (`&&`, `||`) charge no entry tick. Arithmetic forces every operand under the [strict-arithmetic assumption](#strict-arithmetic-assumption); scalar comparisons force both operands, while `&&` and `||` retain short-circuit evaluation.

Unticked operations still pay the ticks of any thunks they force. Costs count source-level function entries, not elapsed wall-clock time or GHC-optimized execution.

For a saturated built-in primitive at a known concrete scalar type, the
translator uses literal operands directly. If a strict use has already forced
a source binding on the current path, a later scalar operation can use that
value at the same type. A lazy consumer still receives the original thunk.
For example, the guard in `take` forces the count; the selected cons branch
can suspend the decrement using that count, while the recursive call stays
inside the tail thunk. These choices preserve the demanded value and its
ticks, but private thunk names, allocations, and usage annotations may differ
from a translation that suspends every operand.

### Laziness and Local Sharing

Translated bindings express evaluation and sharing through the runtime's core monadic and thunk primitives—`lazy`, `force`, `forcing`, `bind`, `ret`, and `tick` (rendered using standard Haskell `do` notation and `pure`)—together with metadata-preserving structural representation views:

- **Call-by-need suspension and reuse**: Function arguments, constructor fields, and local `let`/`where` value bindings are suspended with `lazy` and evaluated at most once per binding instance when demanded with `force`. Passing an existing thunk as an argument or field reuses it directly without allocating a wrapper thunk.
- **Lazy pattern matching and guards**: Variable and wildcard (`_`) patterns preserve the subject thunk's identity without forcing it; in an `as`-pattern (`x@pat`), the alias `x` preserves the subject's thunk identity while matching `pat` forces whatever layers `pat` inspects. Constructor and literal patterns force only the layer they inspect, and untaken equations, guards, or conditional branches neither allocate thunks nor charge ticks. Local bang bindings without class contexts (`let !x = rhs in body`) force the outer value before continuing while leaving constructor fields lazy and sharing `x`.
- **Reading generated decisions**: Adjacent equations whose next pattern inspects a constructor on the same argument share a `case`, even if the function has other arguments. Each branch tries its equations in source order. Guards that can return before the inspection stay before the `case`. A comparison used directly as a condition forces its operands and tests the result with `if`; a final Prelude `otherwise` supplies the alternative.
- **Local polymorphic and dictionary-sensitive sharing**: Each function invocation allocates fresh local thunks. For supported structural result and constructor-payload shapes, unconstrained polymorphic local values share a single native thunk across type instantiations through pure structural views that preserve thunk identities and annotations without forcing unused fields. Constrained local values in supported scopes and shapes share when the type arguments determining their dictionaries agree—including GHC-proven closed scalar predicates such as `Integral Int` and `Integral Integer` when another quantified parameter varies without affecting dictionary choice—allocating one native thunk per distinct dictionary without extra `Typeable` constraints; remaining shapes fall back to the legacy helpers noted in [Limitations](#limitations).

## Supported Source Subset and Generated Support

- **Algebraic datatypes**: `data` declarations with multiple or nullary constructors, type parameters, recursion, nested non-regular recursion (e.g., `data Nested a = End a | More (Nested [a])`), higher-kinded parameters (`data Higher f a = Higher (f a)`), recursive constructor applications (`data Fix f = In (f (Fix f))`), infix constructors, and named record fields.
- **Patterns, guards, and expressions**: Ordered function equations and `case` alternatives using variable, wildcard, constructor, list (`[]`, `:`), boxed pair `(a, b)`, record, `as`, and literal (numeric, `Char`, `String`) patterns; boolean, pattern (`pat <- expr`), `let`, and comma-separated conjunctive guards; `let` and `where` clauses with local functions (including mutually recursive groups) and non-recursive named value bindings; `if-then-else`; `case-of`; `undefined`; literal-string `error "msg"`; and explicit type signatures and expression annotations.
- **Selected library operations**: `map`, `foldr`, `foldl`, `concatMap`, `filter`, `zipWith`, `scanl`, `head`, `tail`, and `(.)` from `Prelude`, plus `repeatly` from `Memorist.Example.NewtonRaphson`. Folds operate over lists, `Maybe`, and boxed pairs via `FoldableT`. Only these selected operations are replaced, and replacements follow resolved names rather than lexical shadowing.
- **Generated datatype and QuickCheck support**: For each source datatype `Tree a`, the generated module defines a thunked representation type (`TreeT`), a demand type (`TreeD` wrapping fields in `ThD` with `Skip` and `Need`), `Repr` and `Demand` instances (`reprM`, `abstract`, `fullDemand`), and budget-bounded QuickCheck demand generators and compatible shrinkers (`genTreeDemand`, `shrinkTreeDemand`). Demand generators emit the outer demand constructor and divide the budget across its fields; each field generator (`genField`, `arbitraryField`) yields `Skip` when its allotted budget is `<= 1`, and otherwise recurses at `budget - 2` (`genDemandBudget` or `resize`) under the runtime's 1:3 `Skip`/`Need` weighting (`liftArbitrary`).

## Limitations

### Strict Arithmetic Assumption

The translator assumes that arithmetic operators (`+`, `-`, `*`, `/`, `div`, `mod`, `quot`, `rem`) and negation are strict in every operand: demanding a result requires evaluating each operand to its outermost value. Haskell's numeric classes, including `Num`, do not enforce this assumption.

For example, a caller-defined `(+)` implemented as `_ + _ = One` ignores both operands. The translation still forces both operands before calling `(+)`, so it may evaluate computations that the source leaves unevaluated, charge extra ticks, or diverge when the source terminates. Non-strict arithmetic instances are outside the supported demand and cost model.

The cost contract also assumes that caller-supplied arithmetic methods obey the same runtime-access restrictions as translated functions: they must not read thunk payloads with `tval` or bypass `force`. Strictness does not grant permission to bypass accounting, and a `Num` constraint alone guarantees neither obligation.

Under these caller obligations, translated polymorphic functions keep their ordinary `Num`, `Fractional`, and `Integral` constraints without requiring a preknown concrete caller type. On non-protected, non-demand operand types, the construction guard accepts the exact Prelude numeric selectors that the translator uses—`+`, `-`, `*`, `negate`, and `fromInteger` from `Num`; `/` and `fromRational` from `Fractional`; and `div`, `mod`, `quot`, and `rem` from `Integral`—when backed by well-formed dictionary evidence. Other numeric methods (`abs`, `signum`, `recip`, `divMod`, `quotRem`) and non-numeric classes (`Eq`, `Ord`, `Show`, `Enum`, `Bounded`) do not receive this exemption and keep their narrower concrete-scalar or stock-demand checks.

### Excluded Source-Global Sharing Costs and Identity

Top-level value bindings translate to monadic actions, and translated functions take no global environment argument. Re-executing a global action does not retain its earlier result or thunk identity across uses:

- **Global sharing costs**: Memorist does not recover source-global sharing costs through direct top-level calls. For instance, the worked `work/shared/f/g/test` program charges 5 ticks at runtime rather than 4 in the source cost model. Global sharing costs are outside the translator's current scope ([issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2#issuecomment-6972277826)); the translator does not add a global cache or pre-evaluate globals to compensate.
- **Global thunk identity**: Repeated executions of a global action also allocate fresh thunks rather than preserving global thunk aliases across calls, even when measured tick counts happen to agree ([issue #4](https://github.com/lastland/hs-monadic-cost-translator/issues/4)). Local sharing, local thunk identities, and per-invocation freshness remain supported.

### Incomplete Six-Combinator Construction-Only Contract

The six-combinator contract restricts ordinary translated computations to `lazy`, `force`, `forcing`, `bind`, `ret`, and `tick` (with `do`/`pure` notation), approved runtime list and iteration helpers, and checked metadata-preserving structural views. Runtime evaluators and cost observers (`eval`, `eval1`, `value`, `runM`, `infcost`, `correct`, and annotation projections) remain caller-side reasoning operations and are forbidden in generated modules.

Generated support declarations receive narrow, role-specific permissions that ordinary computations cannot borrow directly, through helper functions, or through selected class methods:

- **QuickCheck generators and properties**: Definitions whose result is a real QuickCheck `Gen` or `Property` (including local value bindings consumed only by that generator or property) are exempt. Merely taking `Gen` or `Property` as an argument gives no exemption, and ordinary computations cannot consume or force QuickCheck values.
- **Method-specific observation and accounting support**: In generated `Repr` and `Demand` instances, `abstract` and `fullDemand` (and helpers reachable only from them) may read thunk payloads with `tval` and recurse through their respective method, while `usage` (and helpers reachable only from it) may call `usage` and combine thunk accounting sets (`useThunk`, `empty`, `union`). Each permission belongs to its nominal class method: `abstract` and `fullDemand` cannot use accounting operations, `usage` cannot read `tval`, and ordinary computations cannot call `abstract`, `fullDemand`, `usage`, `tval`, or support-only helpers.
- **Standalone value-dependent demand shrinkers**: Value-dependent demand shrinkers in the `Demand`-anchored `GeneratedDemand` class (`shrinkDemandCompatible`), the field shrinker (`shrinkField`), support-only helpers they reach, and pure forwarding wrappers (`shrink*Demand`) are exempt as demand-shrinker support even outside `Gen` and `Property`, together with local values consumed only by a shrinker. Ordinary computations still cannot call demand shrinkers or their helpers, and nested callable or unused ordinary declarations inside support or shrinker definitions remain checked as ordinary computations.

Two gaps still prevent complete generated modules from passing the construction guard:

- **Legacy local sharing fallback**: Local value shapes outside the native lowering rules still emit legacy evaluator helpers (`sharedAt` / `sharedAtBy` using `eval` and `value`) and may require `Typeable` constraints absent from the source signature. In particular, construction-only sharing for potentially overlapping generic instantiations—such as a dictionary-sensitive local used at both an abstract `a` and concrete `Int`, which must share when `a = Int` and stay separate when `a = Integer`—is outside the current scope ([issue #3](https://github.com/lastland/hs-monadic-cost-translator/issues/3)).
- **Common `FoldableT` support and other uncertified class methods**: Every generated module emits the common `FoldableT` class (`foldrT`, `foldlT`, `concatMapT`, and the default `concatMapT` method calling `foldrT`) along with its `ListT`, `MaybeT`, and `PairT` instances. The construction guard does not yet certify `FoldableT` default or given method evidence (such as the 7 `FoldableT` reports in `anf-demand`), or other concrete or generic class methods outside the approved selector families.

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
  Whereas `cost-translate` only typechecks generated candidates against the `MemoristANT` API, the suite's construction guard requires the compiled `MemoristANT` interfaces to match the exact audited runtime module source fingerprints pinned in [`test/ConstructionRuntime.hs`](test/ConstructionRuntime.hs). An API-compatible `hs-pbt/` checkout alone is therefore insufficient: the audited sources must include the [private-allocator patch](patches/README.md) (see that guide to verify an existing checkout) and, if `src/Memorist/Example/ListInf.hs` is unpatched, the tracked [`scanl` laziness patch](.scratch/translator-poc/scanl-demand-fix.patch) (verify with `git apply --reverse --check --directory=hs-pbt .scratch/translator-poc/scanl-demand-fix.patch`, or apply with `git apply --directory=hs-pbt .scratch/translator-poc/scanl-demand-fix.patch` and rebuild `MemoristANT:lib`).

  Before compiling callers, the reference runner checks each generated module and its home imports against the construction-only contract. Because every generated module emits the common `FoldableT` support section (and some modules emit legacy sharing helpers or other uncertified class methods), generated modules can translate and typecheck yet still fail this whole-module guard before reference comparison. The `generated-construction` selector tests the guard itself across construction, structural-view, observation/accounting support, demand-shrinker, and caller-arithmetic controls and expected rejections.
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
