# `cost-translate` Examples

This directory contains twelve pure Haskell modules within the supported subset documented in the [root README](../README.md): seven introductory modules that walk through core translator features and five focused modules that exercise thunk reuse, scoped type annotations, constructor-equation grouping, and `do`-block structure.

### Introductory Modules

| File | Demonstrates |
| :--- | :--- |
| [`Basic.hs`](Basic.hs) | Function entry ticks, `let` sharing, unused bottoms |
| [`Guards.hs`](Guards.hs) | Ordered equations, guards in `case`, literal patterns, short-circuiting |
| [`Lists.hs`](Lists.hs) | `map`, `foldr`, `foldl`, `zipWith`, `scanl`, `filter`, and infinite producers |
| [`Datatypes.hs`](Datatypes.hs) | Recursive and nested datatypes, explicit demands, top-level values |
| [`Records.hs`](Records.hs) | Record fields and selectors, infix constructors, `Maybe` and pair fields |
| [`HigherKinded.hs`](HigherKinded.hs) | Higher-kinded fields, `Fix`, non-regular recursion, conversions |
| [`Shared.hs`](Shared.hs) | Top-level sharing through `initialize`, dictionary-sensitive sharing |

### Focused Behavior and Code-Shape Modules

The root test suite (`cost-translate-test`) translates and tests these five files directly under their `manual-*` selectors:

| Source | Reference-test selector | Demonstrates |
| :--- | :--- | :--- |
| [`ThunkArguments.hs`](ThunkArguments.hs) | `manual-thunk-arguments` | Direct thunk reuse across arguments, fields, and callables vs. computed suspensions |
| [`ThunkMatching.hs`](ThunkMatching.hs) | `manual-thunk-matching` | Thunk identity preservation in `case` and pattern guards, plus polymorphic and dictionary sharing |
| [`ScopedAnnotations.hs`](ScopedAnnotations.hs) | `manual-scoped-annotations` | Scoped type annotations on `case` and guard subjects under shadowed `forall` binders |
| [`ConstructorEquations.hs`](ConstructorEquations.hs) | `manual-constructor-equations` | Single-inspection grouping for adjacent disjoint constructors vs. guard/wildcard barriers |
| [`TerminalDo.hs`](TerminalDo.hs) | `manual-terminal-do` | Flat ordinary `do` blocks for function entry ticks and conditionals vs. scoped annotations |

Run all five automated reference comparisons together with:

```sh
stack test cost-translate:test:cost-translate-test --test-arguments='manual-thunk-arguments manual-thunk-matching manual-scoped-annotations manual-constructor-equations manual-terminal-do'
```

## Setup and Usage

### Building the Translator

Generated modules require a GHC 9.10.3 environment exposing `MemoristANT` and `QuickCheck`. The root Stack configuration provides both when `hs-pbt/` is present (see the [root README](../README.md) for prerequisites):

```sh
stack build
```

### Translating and Inspecting an Example

Translate and compile from the repository root while placing generated modules, logs, and compiler artifacts in a temporary directory `$work`. Because each generated module imports its source module qualified as `Source`, keep `-iexamples` on GHC's search path:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/Basic.hs "$work/Generated.hs"
stack exec -- ghc -v0 -fforce-recomp -package MemoristANT -package QuickCheck \
    -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
```

`cost-translate` writes `Generated.hs.log` next to the output path. If translation fails, `cost-translate` records the diagnostic in the log and exits non-zero without publishing or overwriting `Generated.hs`.

To explore a generated module interactively in GHCi:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/Basic.hs "$work/Generated.hs"
stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
    -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
```

Inside GHCi, import the runtime and define `runInt` to evaluate a `MemoristM Int` computation and report its value and full-demand tick count:

```haskell
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
let runInt :: MemoristM Int -> (Int, Integer); runInt m = (value (eval1 m), MT.toInteger (infcost m fullDemand))
```

### Running a Compiled Driver

A standalone driver checks value equivalence against the pure source module with `correct` and measures demanded ticks with `infcost` (or `infcost'` when passing a concrete demand value instead of a demand function):

```haskell
module Main where

import Generated
import qualified Basic as Source
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT

main :: IO ()
main = do
    print (correct (fromPure1 doubleM (5 :: Int)) (Source.double 5))
    print (MT.toInteger (infcost (fromPure1 doubleM (5 :: Int)) fullDemand))
```

Save the driver as `"$work/Main.hs"` after translating `Basic.hs`, then compile and run it:

```sh
stack exec -- ghc -v0 -package MemoristANT -package QuickCheck \
    -i -i"$work" -iexamples -outputdir "$work/objects" \
    "$work/Main.hs" -o "$work/check-example"
"$work/check-example"
```

It prints `True` and `1`.

Modules with top-level value bindings export `Bindings` and
`initialize :: MemoristM Bindings`. Only functions that depend on those values,
directly or through other functions, take `Bindings` as their first argument.
Other functions retain their ordinary interface and need no initialization.

**Cost model summary**: Every translated function call (and generated record selector) charges 1 tick on entry, constructor allocation charges 0 ticks, and scalar operators add 0 entry ticks. All costs below are measured under `fullDemand` unless stated otherwise.

## Introductory Walkthroughs

### `Basic.hs`

| Call | Value | Cost |
| :--- | :--- | :--- |
| `fromPure1 doubleM (5 :: Int)` | `10` | `1` |
| `fromPure1 sharedM (5 :: Int)` | `20` | `2` |
| `fromPure1 capturedM (5 :: Int)` | `30` | `4` |
| `fromPure1 keepM (5 :: Int)` | `5` | `1` |

`shared` evaluates `double x` once and reuses its thunk across `y + y`. `captured` calls a local function `add` twice while sharing the captured thunk `y` (`1` outer tick + `1` `double` tick + `2` `add` ticks). `keep` never forces its unused bottom binding, so only `keep` ticks.

### `Guards.hs`

| Call | Value | Cost |
| :--- | :--- | :--- |
| `fromPure1 orderedM (5 :: Int)` | `5` | `2` |
| `fromPure1 guardedM (5 :: Int)` | `10` | `1` |
| `fromPure1 observedM (5 :: Int)` | `14` | `4` |
| `fromPure1 numericM (5 :: Int)` | `5` | `2` |
| `fromPure1 keepM (5 :: Int)` | `5` | `1` |

`ordered` skips the `[]` equation and the first guard of the cons equation before taking `otherwise`, never forcing the undefined tail. `guarded` combines a pattern guard and a `let` guard inside `case`. `observed` matches `Char` and `String` literal patterns, `numeric` falls through the numeric pattern `0` to the catch-all equation, and `keep` short-circuits `||` before its undefined right operand.

### `Lists.hs`

| Call | Value | Cost |
| :--- | :--- | :--- |
| `fromPure1 mappedM [5, 5 :: Int]` | `[6, 6]` | `2n + 2 = 6` |
| `fromPure1 foldedM [5, 5 :: Int]` | `10` | `2n + 2 = 6` |
| `fromPure1 summedFromLeftM (5 :: Int)` | `10` | `6` |
| `fromPure1 zippedM (5 :: Int)` | `10` | `4` |
| `fromPure1 scannedM (5 :: Int)` | `5` | `3` |
| `fromPure1 filteredM (5 :: Int)` | `5` | `4` |
| `fromPure1 observeM (5 :: Int)` | `6` | `4` |
| `fromPure1 restM (5 :: Int)` | `6` | `5` |

`zipped`, `scanned`, and `filtered` never force their undefined list tails. `observe` reads the second element of the infinite `stream` producer via pattern matching, and `rest` reads it via `head` after `tail`. Evaluating `stream` under an explicit finite demand (`import Memorist.Data.List` for `ConsD`) charges only for the demanded prefix:

```haskell
infcost' (fromPure1 streamM (5 :: Int))
         (ConsD (Need 5) (Need (ConsD (Need 6) (Need (ConsD (Need 7) Skip)))))
-- Cost 3
```

### `Datatypes.hs`

`Datatypes.hs` defines `Tree`, the non-regular datatype `Nested`, and the infinite
producer `grow`. Its functions do not depend on the top-level value `sample`, so
they are directly callable:

```haskell
totalCheck :: MemoristM Int
totalCheck = fromPure1 totalM (5 :: Int)

-- correct totalCheck (5 :: Int) == True
-- infcost totalCheck fullDemand == Cost 4
```

| Call | Value | Cost |
| :--- | :--- | :--- |
| `totalM` at `5` | `5` | `4` |
| `deepM` at `5` | `11` | `6` |
| `growM` at `5` under `NodeD (Need 5) Skip Skip` | — | `1` |

Constructor application is free, so `grow` charges one tick per demanded node. Source values convert to thunked representations with `reprM` and back with `abstract`:

```haskell
let converted = value (eval1
        (reprM (Source.Node (5 :: Int) Source.Leaf Source.Leaf)
            :: MemoristM (TreeT Int)))
case abstract converted :: Source.Tree Int of
    Source.Node n _ _ -> n  -- 5
```

The generated module also exports QuickCheck demand helpers `genTreeDemand`, `shrinkTreeDemand`, `genNestedDemand`, and `shrinkNestedDemand`.

### `Records.hs`

| Call | Value | Cost |
| :--- | :--- | :--- |
| `fromPure1 recordedM (5 :: Int)` | `10` | `3` |
| `fromPure1 pairedM (5 :: Int)` | `5` | `2` |
| `fromPure1 nestedM (5 :: Int)` | `5` | `3` |

`recorded` never forces the undefined `ignored` field; reading `payload` via the generated record selector `payloadM` charges 1 tick in addition to `recordedM` and `readBoxM`. `paired` matches the infix constructor `:*:` without forcing its second field, and `nested` combines `Maybe` and pair fields inside a non-regular datatype.

### `HigherKinded.hs`

| Call | Value | Cost |
| :--- | :--- | :--- |
| `fromPure1 observedM (5 :: Int)` | `5` | `3` |
| `fromPure1 listedM (5 :: Int)` | `5` | `3` |
| `fromPure1 taggedM (5 :: Int)`, abstracted | `Higher (Layer 5)` | `1` |

`observed` unwraps a `Fix`-shaped list, `listed` matches a `Higher f a` field applied to `[]`, and `tagged` wraps `Layer` inside `Higher`. Higher-kinded values convert between pure and thunked representations through `reprM` and `abstract`:

```haskell
let converted = value (eval1
        (reprM (Source.Higher (Source.Layer (5 :: Int)))
            :: MemoristM (HigherT LayerT Int)))
case abstract converted :: Source.Higher Source.Layer Int of
    Source.Higher (Source.Layer n) -> n  -- 5
```

`forest` (`Fix []`) and `nested` (`Nest []`) are infinite producers that can be tested with finite demands, and `Mixed f a` exercises non-regular recursion through a higher-kinded parameter.

### `Shared.hs`

| Program via `initialize` | Value | Cost |
| :--- | :--- | :--- |
| `firstM` at `5` plus `secondM` at `5` | `30` | `3` |
| `asIntM` at `0` plus `asIntegerM` at `0` | `(1, 2)` | `5` |
| `asIntM` at `0` plus `asIntM` at `0` | `2` | `3` |

```haskell
translated :: MemoristM Int
translated = do
    bindings <- initialize
    a <- fromPure1 (firstM bindings) (5 :: Int)
    b <- fromPure1 (secondM bindings) (5 :: Int)
    pure (a + b)
```

`firstM` and `secondM` share the top-level `seed` thunk in `bindings`, so `double` evaluates once (`1` tick for `firstM` + `1` tick for `secondM` + `1` tick for `doubleM`). The polymorphic binding `shared :: (Eq a, Num a) => a` evaluates `expensive 18446744073709551616`, which overflows to `0` at 64-bit `Int` (returning `1` in `1` tick) and stays non-zero at `Integer` (recursing once to `expensive (n - n)` and returning `2` in `2` ticks). Calls at the same dictionary type (`Int`) share one thunk, while `Int` and `Integer` use separate thunks.

## Focused Behavior and Code-Shape Walkthroughs

For each module below, translate the file into `$work/Generated.hs`, load it in GHCi with the setup imports and `runInt` helper from [Translating and Inspecting an Example](#translating-and-inspecting-an-example), and evaluate the expressions shown. When checking thunk reuse, compare thunk names (`tname a == tname x`) and caller fresh-name progression (`next == nnext (tname x)` from `runM program nzero`).

### `ThunkArguments.hs` (`manual-thunk-arguments`)

`ThunkArguments.hs` contrasts direct thunk references with computed expressions across arguments, data fields, and higher-order callables:

- Passing an existing thunk variable `x`, parenthesized `((x))`, or type-annotated `(x :: a)` to a function, constructor (`Box`), record, tuple, list head, or `(:)` reuses that thunk directly without allocating a wrapper thunk or forcing `x`.
- Computed expressions (`bad x`, `keep x`) and explicit local bindings (`let shared = keep x`) allocate fresh `M.lazy` suspensions that remain unevaluated until demanded.
- Partially applied (`map (pair x) xs`) and composed (`map (pair x . keep) xs`) callables reuse the captured thunk `x` in every pair's first field while suspending `keep` for each element in `composedM`.

In GHCi:

```haskell
runInt (fromPure1 plainM (5 :: Int))
-- (42, 2)
runInt (fromPure1 annotatedM (5 :: Int))
-- (42, 2)
runInt (fromPure1 computedM (5 :: Int))
-- (42, 2)

let (res, next) = runM (do x <- lazy (error "unused" :: MemoristM Int); PairT a b <- tupleM x; pure (tname a == tname x, tname b == tname x, tname x)) nzero
let (sameA, sameB, xName) = value res in (sameA, sameB, next == nnext xName, MT.toInteger (costAnnot res))
-- (True, True, True, 1)
```

### `ThunkMatching.hs` (`manual-thunk-matching`)

`ThunkMatching.hs` exercises thunk identity preservation across `case` expressions and pattern guards (`pat <- expr`):

- Matching an existing thunk reference against a variable (`y`), wildcard (`_`), parenthesized reference, type-annotated reference, or `as`-pattern (`y@_`) preserves the subject's thunk identity (`tname`) without forcing bottom.
- Computed match subjects (`work x`, `bad x`) and explicit `let` bindings (`let bound = x`) allocate fresh `M.lazy` suspensions shared across uses of the bound variable.
- Unconstrained polymorphic values (`sharedEmpty :: [a]`) reuse a single thunk across `[Int]` and `[Bool]` instantiations, while dictionary-constrained values (`shared :: Num a => a`) share across uses of `Int` (`sameCaseM`, `sameGuardM`) and separate `Int` from `Integer` (`differentM`).

`plainM`, `guardAliasM`, and `computedCaseM` do not depend on the top-level
values and need no initialization. `sameCaseM`, `sameGuardM`, and `differentM`
use `shared`, so their calls still use `Bindings`:

```haskell
let (res, next) = runM (do x <- lazy (error "unused" :: MemoristM Int); PairT a b <- plainM x; pure (tname x, tname a, tname b)) nzero
let (xName, aName, bName) = value res in (aName == xName, bName == xName, next == nnext xName, MT.toInteger (costAnnot res))
-- (True, True, True, 1)

let (gRes, gNext) = runM (do x <- lazy (error "unused" :: MemoristM Int); PairT a b <- guardAliasM x; pure (tname x, tname a, tname b)) nzero
let (gxName, gaName, gbName) = value gRes in (gaName == gxName, gbName == gxName, gNext == nnext gxName, MT.toInteger (costAnnot gRes))
-- (True, True, True, 1)

runInt (do PairT a b <- fromPure1 computedCaseM (5 :: Int); (+) <$> force a <*> force b)
-- (20, 2)

value (eval1 (do bindings <- initialize; u <- lazy (pure ()); PairT a b <- sameCaseM bindings u; PairT c d <- sameGuardM bindings u; PairT e f <- differentM bindings u; pure (tname a == tname b, tname a == tname c, tname c == tname d, tname a == tname e, tname e /= tname f)))
-- (True, True, True, True, True)
```

### `ScopedAnnotations.hs` (`manual-scoped-annotations`)

`ScopedAnnotations.hs` exercises `case` and pattern-guard subjects carrying scoped type annotations (`(x :: a)` and `(shared :: a)`) inside an outer `forall a` when a nested local function declares a shadowing `forall a` (`inner :: forall a. a -> Int`) or a distinct `forall b`:

- The generated code binds the annotated subject in a plain Haskell `let` (`let caseThunk0 = (x :: M.Th a)`) at the match boundary outside the inner `forall a`, preventing the inner binder from capturing the outer type variable `a`.
- Because `caseThunk0` is a plain Haskell alias rather than `M.lazy (M.force x)`, it preserves `tname`, adds no thunk allocation, force, or tick, leaves unused bottom arguments suspended, and keeps the outer dictionary selection (`Int` vs. `Integer`).

The parameter-only functions are directly callable. The factory functions refer
to `shared` and still require `Bindings`:

```haskell
runInt (fromPure1 caseParameterM (5 :: Int))
-- (1, 3)
runInt (fromPure1 guardParameterM (5 :: Int))
-- (1, 3)
runInt (fromPure1 caseParameterM (undefined :: Int))
-- (1, 3)
runInt (fromPure1 guardParameterM (undefined :: Int))
-- (1, 3)

runInt (do bindings <- initialize; fromPure1 (caseFactoryM bindings) (5 :: Int))
-- (1, 3)
runInt (do bindings <- initialize; fromPure1 (guardFactoryM bindings) (5 :: Int))
-- (1, 3)
runInt (do bindings <- initialize; fromPure1 (caseFactoryM bindings) (5 :: Integer))
-- (2, 4)
runInt (do bindings <- initialize; fromPure1 (guardFactoryM bindings) (5 :: Integer))
-- (2, 4)
```

### `ConstructorEquations.hs` (`manual-constructor-equations`)

`ConstructorEquations.hs` demonstrates how multi-equation functions compile to constructor inspections and ordered fallbacks:

- Contiguous unary equations with distinct outer constructors (`choose` on `[]` and `y : _`) share a single outer `M.force` and a single `case` expression with `L.NilT` and `L.ConsT` branches.
- Wildcard and variable equations (`wildcard`, `variable`, and the local unary `pick` helpers in `guardedWildcard` and `guardedVariable`) and repeated constructors (`repeated`) act as ordering barriers so guards execute before any constructor inspection from later equations.

Translate `examples/ConstructorEquations.hs` and inspect `chooseM` in `"$work/Generated.hs"`:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/ConstructorEquations.hs "$work/Generated.hs"
rg -n -A 18 '^chooseM ' "$work/Generated.hs"
```

In GHCi:

```haskell
runInt (fromPure1 chooseM ([] :: [Int]))
-- (0, 1)
runInt (fromPure1 chooseM [5 :: Int])
-- (5, 2)
runInt (fromPure1 orderedM (5 :: Int))
-- (5, 3)
runInt (fromPure2 wildcardM True (undefined :: [Int]))
-- (7, 1)
runInt (fromPure2 guardedWildcardM True (undefined :: [Int]))
-- (7, 2)
runInt (fromPure2 guardedVariableM True (undefined :: [Int]))
-- (7, 3)
runInt (fromPure2 guardedWildcardM False ([] :: [Int]))
-- (0, 2)
runInt (fromPure2 guardedVariableM False (5 : undefined :: [Int]))
-- (5, 2)
```

### `TerminalDo.hs` (`manual-terminal-do`)

`TerminalDo.hs` demonstrates `do`-block splicing and scope boundaries for conditionals:

- In `choose c x y = if c then x else y`, the function entry `M.tick`, the `M.force` of the condition thunk, and the `if-then-else` share a single flat `do` block (`chooseM`).
- When an explicit type annotation encloses the conditional (`annotated`), the generated `do` block preserves the enclosing `:: M.MemoristM a` annotation and keeps the `else` branch's local `saved` binding scoped to that branch (`annotatedM`).

Translate `examples/TerminalDo.hs` and inspect `chooseM`, `localM`, and `annotatedM`:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/TerminalDo.hs "$work/Generated.hs"
rg -n -A 12 '^(chooseM|localM|annotatedM) ' "$work/Generated.hs"
```

In GHCi:

```haskell
runInt (fromPure3 chooseM True (5 :: Int) (undefined :: Int))
-- (5, 1)
runInt (fromPure2 ignoredFailureM True (5 :: Int))
-- (10, 3)
```

The generated `localM` keeps its shared `saved` binding in a `let` statement
inside the outer `do`. The entry tick, thunk allocations, and final call share
one block; type annotations and branch-local scopes remain intact.

## Source Conventions and Limitations

The example modules use syntactic forms required by the translator's supported subset:

- **Eta-expanded function bindings**: Definitions such as `mapped xs = map bump xs` are written with explicit parameters because a point-free binding `mapped = map bump` has syntactic arity 0 and would be treated as a value binding rather than a unary function.
- **Supported declarations and expressions**: Single-constructor wrappers use `data` (`newtype` is unsupported), conditionals use `if-then-else` (list comprehensions are unsupported), and negation in `ConstructorEquations.hs` uses `0 - y` because named `negate` is not a supported callable function head.
- **Warning and lint checks on `manual-*` sources**: When any `manual-*` selector runs, the test suite compiles its source file with `ghc -fno-code -Wall -Werror` and lints it with `hlint` (requiring `hlint` on `PATH`). Those five files include targeted `OPTIONS_GHC` and `HLINT` pragmas where the tested syntax intentionally shadows type variables (`ScopedAnnotations.hs`), parenthesizes references or annotations (`ThunkArguments.hs`, `ThunkMatching.hs`, `ScopedAnnotations.hs`), or uses `case`/`as`-patterns that HLint would otherwise simplify.
- **Generated output**: Candidates are typechecked before publication. Warning and lint checks cover representative fixtures, not every generated module; arbitrary generated output is not guaranteed to pass `-Wall -Werror` or HLint.
