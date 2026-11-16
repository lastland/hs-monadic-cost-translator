# cost-translate examples

Seven introductory modules and five regression modules for trying the translator. Each module
stays inside the supported subset described in the
[root README](../README.md), and every value and cost quoted below was
checked by translating the module, compiling the generated output, and
running a handwritten driver against it.

| File | Demonstrates |
| --- | --- |
| `Basic.hs` | Function ticks, `let` sharing, unused bottoms |
| `Guards.hs` | Ordered equations, guards in `case`, literal patterns, short-circuiting |
| `Lists.hs` | `map`/`foldr`/`foldl`/`zipWith`/`scanl`/`filter` with named callbacks, infinite producers |
| `Datatypes.hs` | Recursive and nested datatypes, explicit demands, top-level values |
| `Records.hs` | Record fields and selectors, infix constructors, `Maybe` and pair fields |
| `HigherKinded.hs` | Higher-kinded fields, `Fix`, non-regular recursion, conversion |
| `Shared.hs` | Top-level sharing through `initialize`, dictionary-sensitive sharing |

## Fixes since main

These examples cover the five translator fixes in the original reviewed range
`f42170ecc5455cb5926f892d12f8e2024a9e7f76..4828860473da4f9d2ce457da29215972784c3cdf`.
The table pins the range rather than following a moving `main` branch.
See [the validation record](VALIDATION.md) for review findings and pre-fix checks.

| Fix | Source | Reference-test selector |
| --- | --- | --- |
| `b5f21ad` | [ThunkArguments.hs](ThunkArguments.hs) | `manual-thunk-arguments` |
| `446f2c1` | [ThunkMatching.hs](ThunkMatching.hs) | `manual-thunk-matching` |
| `f615f65` | [ScopedAnnotations.hs](ScopedAnnotations.hs) | `manual-scoped-annotations` |
| `dcf55d1` | [ConstructorEquations.hs](ConstructorEquations.hs) | `manual-constructor-equations` |
| `d892e2f` | [TerminalDo.hs](TerminalDo.hs) | `manual-terminal-do` |

The reference suite translates these exact files, not copies under `test/cases/`.
Every test invocation checks that each source, reference case, and table row
exists. `fixture-policy` also checks that missing sources, copied sources,
missing registrations, and missing instructions are rejected.

Run their automated checks together:

```sh
stack test cost-translate:test:cost-translate-test --test-arguments='manual-thunk-arguments manual-thunk-matching manual-scoped-annotations manual-constructor-equations manual-terminal-do'
```

After [setup](#setup), translate and inspect any of these modules without writing files
into `examples/`, place the generated module, log, and GHC compiler artifacts
in a fresh temporary directory `$work`:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/ThunkArguments.hs "$work/Generated.hs"
stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
    -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
```

Inside GHCi, import the runtime and define a monomorphic `Int` helper so
`fullDemand` has a concrete `Demand Int` dictionary:

```haskell
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
let runInt :: MemoristM Int -> (Int, Integer); runInt m = (value (eval1 m), MT.toInteger (infcost m fullDemand))
```

When checking thunk reuse in GHCi, compare thunk identities (`tname a == tname x`)
and caller-level fresh-name progression (`next == nnext (tname x)` from
`runM program nzero`) rather than hardcoding internal `Name` constants.
`ThunkMatching.hs` and `ScopedAnnotations.hs` define top-level value bindings,
so their generated modules export `Bindings` and `initialize :: MemoristM Bindings`,
and every generated function takes `bindings` as its first argument.

### `ThunkArguments.hs` (`b5f21ad`, `manual-thunk-arguments`)

- **Root cause**: `emitExpr` and `emitCallable` previously routed every function
  argument, constructor/record/tuple/list field, and captured callable argument
  through `lazyArguments`, wrapping even an existing thunk variable in a fresh
  `M.lazy (M.force x)` suspension.
- **Before and after**: Before `b5f21ad`, forwarding `x`, `((x))`, or `(x :: a)`
  allocated a forwarding thunk with a new `tname` and advanced the caller's
  fresh-name counter. After `b5f21ad`, `emitThunk` reuses resolved thunk
  references directly (including through parentheses and non-fresh type
  annotations) without allocating or forcing, while computed arguments
  (`bad x`, `keep x`) and explicit local bindings (`let shared = keep x`) still
  allocate `M.lazy` suspensions. Source function entry ticks remain unchanged.
- **Manual translation and GHCi checks (`x = 5 :: Int`)**:

  ```sh
  work=$(mktemp -d)
  stack run cost-translate -- examples/ThunkArguments.hs "$work/Generated.hs"
  stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
      -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
  ```

  In GHCi (after the setup imports and `runInt` definition above):

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

  `plainM`, `parenthesizedM`, `annotatedM`, and `computedM` each return `42` in
  `2` ticks (`1` for the outer function and `1` for `ignoreM`; `computedM`
  suspends `badM x` without evaluating it). `tupleM`, `constructorM`, and
  `recordM` retain the input thunk in both fields without allocating a wrapper.
  `listM` reuses it for both heads but allocates the list's tail computations;
  `consM` reuses its distinct head and tail inputs. `partialM` reuses the captured
  input in each pair's first field. `composedM` does the same, but suspends the
  real `keep` computation for each second field. `localM` retains one suspension
  for the explicit `let shared = keep x` binding. These genuine suspensions
  must not be mistaken for redundant forwarding wrappers.

### `ThunkMatching.hs` (`446f2c1`, `manual-thunk-matching`)

- **Root cause**: `HsCase` and pattern-guard `BindStmt` (`pat <- expr`)
  unconditionally suspended the match subject in a fresh `M.lazy` thunk, and
  `matchPatterns` threaded `String` variable names rather than generated
  `O.Expr` references.
- **Before and after**: Before `446f2c1`, matching a variable, parenthesized or
  annotated reference, or shared top-level/local value in `case` or a pattern
  guard bound pattern aliases to a fresh wrapper thunk, changing `tname` and
  breaking identity reuse across annotated polymorphic or dictionary-constrained
  bindings (`shared`, `sharedEmpty`). After `446f2c1`, `HsCase` and `BindStmt`
  prepare subjects with `emitThunk` and pass `O.Expr` references through
  `matchPatterns`: variable (`y`), wildcard (`_`), parenthesized, and
  `as`-pattern (`y@_`) matches preserve the subject's thunk identity without
  forcing bottom, while computed subjects (`work x`, `bad x`) and explicit
  `let bound = x` anchors still allocate `M.lazy` suspensions.
- **Manual translation and GHCi checks (`x = 5 :: Int`)**:

  ```sh
  work=$(mktemp -d)
  stack run cost-translate -- examples/ThunkMatching.hs "$work/Generated.hs"
  stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
      -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
  ```

  Because `ThunkMatching.hs` defines top-level values (`shared` and
  `sharedEmpty`), initialize `Bindings` first and pass `bindings` as the first
  argument:

  ```haskell
  let (res, next) = runM (do bindings <- initialize; x <- lazy (error "unused" :: MemoristM Int); PairT a b <- plainM bindings x; pure (tname x, tname a, tname b)) nzero
  let (xName, aName, bName) = value res in (aName == xName, bName == xName, next == nnext xName, MT.toInteger (costAnnot res))
  -- (True, True, True, 1)

  let (gRes, gNext) = runM (do bindings <- initialize; x <- lazy (error "unused" :: MemoristM Int); PairT a b <- guardAliasM bindings x; pure (tname x, tname a, tname b)) nzero
  let (gxName, gaName, gbName) = value gRes in (gaName == gxName, gbName == gxName, gNext == nnext gxName, MT.toInteger (costAnnot gRes))
  -- (True, True, True, 1)

  runInt (do bindings <- initialize; PairT a b <- fromPure1 (computedCaseM bindings) (5 :: Int); (+) <$> force a <*> force b)
  -- (20, 2)

  value (eval1 (do bindings <- initialize; u <- lazy (pure ()); PairT a b <- sameCaseM bindings u; PairT c d <- sameGuardM bindings u; PairT e f <- differentM bindings u; pure (tname a == tname b, tname a == tname c, tname c == tname d, tname a == tname e, tname e /= tname f)))
  -- (True, True, True, True, True)
  ```

  `plainM` and `guardAliasM` (along with their parenthesized, annotated, and
  `as`-pattern variants) keep the input thunk identity in both pair fields
  without forcing bottom. `computedCaseM`, `computedGuardM`, and `localM`
  suspend `work 5` once and share that thunk across both fields (returning
  `10 + 10 = 20` in `2` ticks). `sameCaseM` and `sameGuardM` reuse the same
  `Int` dictionary thunk for `shared`, whereas `differentM` keeps the `Int` and
  `Integer` specializations at distinct thunk identities (`tname e /= tname f`).

### `ScopedAnnotations.hs` (`f615f65`, `manual-scoped-annotations`)

- **Root cause**: After `446f2c1` inlined reused `case` and pattern-guard
  subjects into pattern aliases, a subject carrying a scoped type annotation
  with free type variables—such as `(x :: a)` or `(shared :: a)` inside an
  outer `forall a`—was substituted directly into the body. When a nested local
  binding declared `inner :: forall a. a -> Int`, the inner `forall a` captured
  the free `a` in the inlined `M.Th a` annotation.
- **Before and after**: Before `f615f65`, the parameter and factory examples
  failed external candidate typechecking (`Expected: M.Th a1; Actual: M.Th a`).
  Simply discarding the annotation would risk
  selecting the wrong dictionary specialization instead. After `f615f65`,
  `withMatchThunk` binds a reused subject whose
  annotation contains free target type variables in a plain Haskell `let`
  (`let caseThunk0 = (x :: M.Th a)`) at the match boundary, outside the inner
  `forall a`. Because this binding is a plain `let` rather than
  `M.lazy (M.force x)`, it preserves `tname`, adds no Memorist thunk allocation,
  force, or tick, leaves unused bottoms suspended, and retains the outer `a`
  and its dictionary.
- **Manual translation and GHCi checks (`x = 5`)**:

  ```sh
  work=$(mktemp -d)
  stack run cost-translate -- examples/ScopedAnnotations.hs "$work/Generated.hs"
  stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
      -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
  ```

  `ScopedAnnotations.hs` defines the top-level value `shared`, so pass
  `bindings <- initialize` as the first argument:

  ```haskell
  runInt (do bindings <- initialize; fromPure1 (caseParameterM bindings) (5 :: Int))
  -- (1, 3)
  runInt (do bindings <- initialize; fromPure1 (guardParameterM bindings) (5 :: Int))
  -- (1, 3)
  runInt (do bindings <- initialize; fromPure1 (caseParameterM bindings) (undefined :: Int))
  -- (1, 3)
  runInt (do bindings <- initialize; fromPure1 (guardParameterM bindings) (undefined :: Int))
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

  `caseParameterM` and `guardParameterM` return `1` at cost `3` (`1` outer tick +
  `1` `inner` tick + `1` `ignoreM` tick) even when the input is bottom. In
  `shared = work 18446744073709551616`, the literal wraps to `0` at 64-bit `Int`
  (`work 0 == 1` in `1` tick) and stays nonzero at `Integer` (`work n` recurses
  once to `work (n - n)` and returns `2` in `2` ticks), so `caseFactoryM` and
  `guardFactoryM` return `1` at cost `3` at `Int` and `2` at cost `4` at
  `Integer`.

### `ConstructorEquations.hs` (`dcf55d1`, `manual-constructor-equations`)

- **Root cause**: `emitMatches` previously compiled each function equation
  independently with its own `M.force` and failure continuation
  (`nextEquation`), forcing the outer parameter thunk once per equation when
  control fell through adjacent unary constructor equations.
- **Before and after**: Before `dcf55d1`, `chooseM` forced its list argument in
  the `[]` equation and, on a non-empty list, forced the same list thunk a
  second time in a separate `case` for `y : _`. After `dcf55d1`,
  `adjacentConstructors` groups contiguous unary equations with distinct outer
  constructors into a single outer `M.force` and a single combined `case` with
  `L.NilT` and `L.ConsT` alternatives, while keeping wildcard/variable equations
  (`wildcard`, `variable`) and repeated constructors (`repeated`) as ordering
  barriers so forces never cross guards or catch-all equations.
- **Manual translation, code inspection, and GHCi checks (`x = 5 :: Int`)**:

  ```sh
  work=$(mktemp -d)
  stack run cost-translate -- examples/ConstructorEquations.hs "$work/Generated.hs"
  rg -n -A 18 '^chooseM ' "$work/Generated.hs"
  stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
      -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
  ```

  Inspecting `chooseM` in `"$work/Generated.hs"` shows one direct outer
  `M.force` of the list parameter and one `case` expression containing both
  `L.NilT` and `L.ConsT` branches (checked automatically by
  `checkOuterListInspection` in `test/GeneratedChecks.hs`). In GHCi:

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

  `guardedWildcard` and `guardedVariable` each contain a local **unary** `pick`
  function that captures the flag. Its successful guard must run before any
  constructor inspection, so a bottom list stays unused. With `False`, matching
  reaches the adjacent `[]`/cons equations. The variable success path also calls
  the local `ignoreList` function, which accounts for its extra tick.

### `TerminalDo.hs` (`d892e2f`, `manual-terminal-do`)

- **Root cause**: `Cost.Translate.Output.doE` unconditionally wrapped its body
  in `mkLastStmt`, even when the body was already a bare ordinary `do` block
  (`HsDo _ (DoExpr Nothing) _`), emitting a nested terminal `do` block whenever
  `emitBind` prepended the function entry `M.tick` to a `do`-producing body such
  as `if c then x else y`.
- **Before and after**: Before `d892e2f`, `chooseM` emitted an outer `do` block
  for `M.tick` whose final statement was a nested ordinary `do` block for
  forcing `c` and branching. After `d892e2f`, `O.doE` splices the statements of
  a directly exposed terminal ordinary `do` block into the enclosing `do` block
  so the entry tick and conditional share one ordinary `do` block, while
  parentheses, type annotations (`annotated`), `let` scopes, and other `do`
  flavours remain boundaries and retain their scope and annotations.
- **Manual translation, code inspection, and GHCi checks (`x = 5 :: Int`)**:

  ```sh
  work=$(mktemp -d)
  stack run cost-translate -- examples/TerminalDo.hs "$work/Generated.hs"
  rg -n -A 12 '^(chooseM|annotatedM) ' "$work/Generated.hs"
  stack exec -- ghci -ignore-dot-ghci -v0 -package MemoristANT -package QuickCheck \
      -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
  ```

  Inspecting `"$work/Generated.hs"` confirms that `chooseM` places
  the entry tick, the condition's force, and the conditional in a single
  ordinary `do` block (checked by
  `checkTerminalDoBlocks` in `test/GeneratedChecks.hs`), while `annotatedM`
  retains its enclosing `:: M.MemoristM a` type annotation and keeps the `else`
  branch's `saved` binding scoped to that branch. In GHCi:

  ```haskell
  runInt (fromPure3 chooseM True (5 :: Int) (undefined :: Int))
  -- (5, 1)
  runInt (fromPure2 ignoredFailureM True (5 :: Int))
  -- (10, 3)
  ```

### Other commits in the reviewed range

The remaining 10 commits in
`f42170ecc5455cb5926f892d12f8e2024a9e7f76..4828860473da4f9d2ce457da29215972784c3cdf`
modify test infrastructure, reference-comparison coverage, static policy gates,
CI workflows, and documentation rather than translator code generation:

- `625b2e9` — records ANF validation and adds demanded defaulted case/guard
  alias coverage sharing one `Integer` specialization across calls.
- `910785c` — moves embedded test program strings into checked-in fixture files
  under `test/cases/`, packaged via Cabal `data-files`.
- `dde0667` — adds the runtime-free GitHub Actions CI workflow (HLint,
  `-Wall -Werror`, and runtime-free diagnostic/policy selectors).
- `7c94f7e` — enforces the single-registry CLI selector gate (`test-selection`)
  and GHC-parsed runner file-write policy (`fixture-policy`), and declares MIT
  licensing.
- `815e557` — strengthens ANF caller-allocation assertions, skipped-field
  failing-input demands, and paired `Shadowed.hs`/`Renamed.hs` sources in
  `anf-scoped-renaming`.
- `0b0d780` — adds the isolated `generated/` vs. `reference/` comparison harness
  and `reference-harness` fault-injection self-tests.
- `88c52d8` — adds handwritten `Reference.hs` implementations and static
  `Case.hs` probe catalogs across semantic fixtures.
- `c58b5f4` — adds generated-demand plan and shrink replay (`DemandSupport.hs`)
  and completes polymorphic-sharing, conversion, failure, and snapshot coverage.
- `f737b08` — enforces startup inventory, AST/import whitelist, and Cabal
  manifest validation (`test/ReferencePolicy.hs`) across every fixture and
  documents the architecture in `docs/testing.md`.
- `4828860` — expands CI and local HLint checks to shared test infrastructure
  (`test/*.hs` and `test/reference/*.hs`).

The negative `.hs` specimens under `test/cases/fixture-policy-*`,
`test/cases/diagnostics-*`, `test/cases/whole-module-failure/`,
`test/reference-policy/`, and `test/reference-selftest/` are parser, policy,
diagnostic-rejection, and harness fault-injection inputs—not valid translator
source modules. Exercise these infrastructure and regression checks via the test
runner and validation scripts:

```sh
# Selector and static fixture/reference policy gates (7c94f7e, f737b08)
stack test cost-translate:test:cost-translate-test --test-arguments='test-selection fixture-policy'

# Comparator and observer fault-injection self-tests (0b0d780, c58b5f4)
stack test cost-translate:test:cost-translate-test --test-arguments='reference-harness'

# Selected semantic and diagnostic regression suites (625b2e9, 910785c, 815e557, 88c52d8, c58b5f4, f737b08)
stack test cost-translate:test:cost-translate-test --test-arguments='anf-demand thunk-matching-sharing anf-properties anf-scoped-renaming ordered-equations-and-conjunctive-guards'
stack test cost-translate:test:cost-translate-test --test-arguments='diagnostics whole-module-failure generated-module-name-diagnostic'

# Lint and runtime-free Cabal package validation (910785c, dde0667, 4828860)
hlint src app test/*.hs test/reference/*.hs integration/probes integration/test-integration
python3 test/compiler_preflight.py
sh scripts/validate.sh
```

## Setup

Generated modules need a GHC environment exposing `MemoristANT` and
`QuickCheck`. The root Stack project provides it by registering the locally
supplied runtime checkout at `hs-pbt/` (see the root README for its
prerequisites):

```sh
stack build
```

## Translating an example

Translate and compile from the repository root, keeping all generated files,
logs, and compiler objects inside `$work`. The generated module imports the
source module, so the examples directory stays on the include path:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/Basic.hs "$work/Generated.hs"
stack exec -- ghc -v0 -fforce-recomp -package MemoristANT -package QuickCheck \
    -i -i"$work" -iexamples -outputdir "$work/objects" "$work/Generated.hs"
```

`cost-translate` writes `Generated.hs.log` next to the output. A failed
translation does not publish an output file, but an older successful output
is not removed either: check the exit status and the log.

## Running checks against the output

A minimal driver compares generated values against the pure source and
measures the demanded cost. `correct :: m (Rep a) -> a -> Bool` takes the
generated program and the source-level expected value; `infcost` takes a
demand, here always `fullDemand` (its variant `infcost'` takes an
already-built demand value instead of a demand function):

```haskell
module Main where

import Generated
import qualified Basic as Source
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT

main :: IO ()
main = do
    print (correct (fromPure1 doubleM (5 :: Int)) (10 :: Int))
    print (MT.toInteger (infcost (fromPure1 doubleM (5 :: Int)) fullDemand))
```

Compile a driver placed in `$work` with both `$work` and `examples` on the
include path and object files redirected to `$work/objects`
(`-package MemoristANT -package QuickCheck -i -i"$work" -iexamples -outputdir "$work/objects"`)
and run it. If the source module has top-level value
bindings, the output exports a `Bindings` record and every generated
function takes it as its first argument; see `Shared.hs` below.

Cost policy recap: every translated function invocation ticks once,
constructor application is free, and scalar arithmetic adds no entry ticks.
All costs below are source-level costs under `fullDemand` unless stated
otherwise.

## Basic.hs

| Call | Value | Cost |
| --- | --- | --- |
| `fromPure1 doubleM (5 :: Int)` | 10 | 1 |
| `fromPure1 sharedM (5 :: Int)` | 20 | 2 |
| `fromPure1 capturedM (5 :: Int)` | 30 | 4 |
| `fromPure1 keepM (5 :: Int)` | 5 | 1 |

`shared` demands `double x` once and reuses the shared thunk; `captured`
adds a local function capturing that thunk. `keep` never demands its
unused bottom, so only `keep` itself ticks.

## Guards.hs

| Call | Value | Cost |
| --- | --- | --- |
| `fromPure1 orderedM (5 :: Int)` | 5 | 2 |
| `fromPure1 guardedM (5 :: Int)` | 10 | 1 |
| `fromPure1 observedM (5 :: Int)` | 14 | 4 |
| `fromPure1 numericM (5 :: Int)` | 5 | 2 |
| `fromPure1 keepM (5 :: Int)` | 5 | 1 |

`ordered` passes the `[]` equation and the cons equation's first guard
before reaching `otherwise`, never forcing the undefined tail. `guarded`
combines a pattern guard and a `let` guard inside `case`. `observed`
matches `Char` and `String` literal patterns, and `numeric` falls through
the numeric literal pattern `0` to the catch-all equation. `keep`
short-circuits `||` before its undefined right operand.

## Lists.hs

| Call | Value | Cost |
| --- | --- | --- |
| `fromPure1 mappedM [5, 5 :: Int]` | `[6, 6]` | 2n+2 = 6 |
| `fromPure1 foldedM [5, 5 :: Int]` | 10 | 2n+2 = 6 |
| `fromPure1 summedFromLeftM (5 :: Int)` | 10 | 6 |
| `fromPure1 zippedM (5 :: Int)` | 10 | 4 |
| `fromPure1 scannedM (5 :: Int)` | 5 | 3 |
| `fromPure1 filteredM (5 :: Int)` | 5 | 4 |
| `fromPure1 observeM (5 :: Int)` | 6 | 4 |
| `fromPure1 restM (5 :: Int)` | 6 | 5 |

`zipped`, `scanned` and `filtered` never force their undefined list tails;
`observe` reads the second cell of the infinite `stream` producer, and
`rest` reads it through `head` after `tail`. `stream`
under an explicit finite demand (`import Memorist.Data.List` for `ConsD`)
charges exactly the demanded cells:

```haskell
infcost' (fromPure1 streamM (5 :: Int))
         (ConsD (Need 5) (Need (ConsD (Need 6) (Need (ConsD (Need 7) Skip)))))
-- Cost 3
```

## Datatypes.hs

`Tree`, a nested parameter datatype `Nested`, and the infinite producer
`grow`. The module has a top-level value (`sample`), so every generated
function is gated behind `Bindings`:

```haskell
totalCheck :: MemoristM Int
totalCheck = do
    bindings <- initialize
    fromPure1 (totalM bindings) (5 :: Int)

-- correct totalCheck (5 :: Int) == True
-- infcost totalCheck fullDemand == Cost 4
```

| Call | Value | Cost |
| --- | --- | --- |
| `totalM` at 5 (via `initialize`) | 5 | 4 |
| `deepM` at 5 (via `initialize`) | 11 | 6 |
| `growM` at 5 under `NodeD (Need 5) Skip Skip` | — | 1 |

Constructor application is free: `grow` charges one tick per demanded node.
Source values convert to their representations with `reprM` and back with
`abstract`:

```haskell
let converted = value (eval1
        (reprM (Source.Node (5 :: Int) Source.Leaf Source.Leaf)
            :: MemoristM (TreeT Int)))
case abstract converted :: Source.Tree Int of
    Source.Node n _ _ -> n  -- 5
```

The output also emits demand generators and shrinkers, such as
`genTreeDemand 20 result` and `shrinkTreeDemand result`, for
result-compatible QuickCheck demands (see the root README's datatype
section).

## Records.hs

| Call | Value | Cost |
| --- | --- | --- |
| `fromPure1 recordedM (5 :: Int)` | 10 | 3 |
| `fromPure1 pairedM (5 :: Int)` | 5 | 2 |
| `fromPure1 nestedM (5 :: Int)` | 5 | 3 |

`recorded` never demands the undefined `ignored` field; reading `payload`
via the record selector adds its own tick. `:*:` shows an infix constructor
with a lazy second field, and `nested` mixes `Maybe` and pair fields.

## HigherKinded.hs

| Call | Value | Cost |
| --- | --- | --- |
| `fromPure1 observedM (5 :: Int)` | 5 | 3 |
| `fromPure1 listedM (5 :: Int)` | 5 | 3 |
| `fromPure1 taggedM (5 :: Int)`, abstracted | `Higher (Layer 5)` | 1 |

`observed` unwraps a `Fix`-shaped list; `listed` matches a `Higher f a`
field applied to a list; `tagged` wraps a `Layer` inside `Higher`, and its
result roundtrips back to a source value. Source values convert to their
representations with `reprM` and back with `abstract`:

```haskell
let converted = value (eval1
        (reprM (Source.Higher (Source.Layer (5 :: Int)))
            :: MemoristM (HigherT LayerT Int)))
case abstract converted :: Source.Higher Source.Layer Int of
    Source.Higher (Source.Layer n) -> n  -- 5
```

`forest` (a `Fix` over lists) and `nested` (a `Nest` over `Maybe`) are
infinite producers whose bounded demands work like `grow` above. The
non-regular `Mixed f a` recurses through its higher-kinded field; the
output generates `MixedT` and `genMixedDemand`/`shrinkMixedDemand` helpers
for all of these.

## Shared.hs

| Program via `initialize` | Value | Cost |
| --- | --- | --- |
| `firstM` at 5 plus `secondM` at 5 | 30 | 3 |
| `asIntM` at 0 plus `asIntegerM` at 0 | `(1, 2)` | 5 |
| `asIntM` at 0 plus `asIntM` at 0 | 2 | 3 |

```haskell
translated :: MemoristM Int
translated = do
    bindings <- initialize
    a <- fromPure1 (firstM bindings) (5 :: Int)
    b <- fromPure1 (secondM bindings) (5 :: Int)
    pure (a + b)
```

The two calls share the top-level `seed`, so `double` ticks once. The
`Num`-constrained `shared` value computes `1` at `Int` (overflow) and `2`
at `Integer`: different dictionaries split sharing, and repeated uses of
the same dictionary share.

## Conventions in these files

The source examples intentionally use shapes the translator accepts even where
idiomatic Haskell would differ: eta-expanded named functions such as
`mapped xs = map bump xs` (a point-free `mapped = map bump` would turn a
function binding into a value binding), plain `data` for single-constructor
types (`newtype` is outside the subset), and `if`/`then`/`else` instead of
comprehensions:

- **Introductory source modules (`Basic.hs` through `Shared.hs`)**: omit inline
  `HLINT` pragmas so the examples stay readable; they remain outside the
  repository's source `hlint` command line.
- **Regression source modules (`ThunkArguments.hs`, `ThunkMatching.hs`,
  `ScopedAnnotations.hs`, `ConstructorEquations.hs`, `TerminalDo.hs`)**:
  `checkManualSourceWarnings` in `test/GeneratedChecks.hs` compiles each source
  file with `ghc -fno-code -Wall -Werror` and lints it with `hlint` (requiring
  `hlint` on `PATH`) whenever its `manual-*` selector runs. Narrow inline
  pragmas document the exact syntactic shapes under test:
  `ScopedAnnotations.hs` enables `-Wno-name-shadowing` for its four
  `inner :: forall a. a -> Int` shadowing regressions and ignores
  `Redundant bracket` on parenthesized scoped annotations;
  `ThunkArguments.hs` ignores `Eta reduce` and `Redundant bracket`;
  `ThunkMatching.hs` ignores `Redundant bracket`, `Redundant as-pattern`, and
  `Redundant case`; and `ConstructorEquations.hs` ignores `Use negate` because
  named `negate` is outside the supported calling conventions.
- **Generated modules**: use labeled common and module-specific section banners,
  blank-line-separated top-level declarations, and grouped alphabetical imports;
  source binders retain their names (with primes added only to avoid collisions)
  while generated temporaries use short positional names. Every translated
  candidate is verified with `ghc -fno-code` before publication, while automated
  `-Wall -Werror`, HLint, and layout checks on generated outputs remain bounded
  to the representative `generated-warnings` and `generated-warnings-minimal`
  fixtures in `test/GeneratedChecks.hs` (not all generated outputs are claimed
  to be warning-free).
