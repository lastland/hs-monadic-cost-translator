# cost-translate examples

Seven self-contained source modules for trying the translator. Each module
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

## Setup

Generated modules need a GHC environment exposing `MemoristANT` and
`QuickCheck`. The root Stack project provides it by registering the locally
supplied runtime checkout at `hs-pbt/` (see the root README for its
prerequisites):

```sh
stack build
```

## Translating an example

Translate and compile from the repository root. The generated module imports
the source module, so the examples directory stays on the include path:

```sh
work=$(mktemp -d)
stack run cost-translate -- examples/Basic.hs "$work/Generated.hs"
stack exec -- ghc -v0 -fforce-recomp -package MemoristANT -package QuickCheck \
    -iexamples "$work/Generated.hs"
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
include path (`-package MemoristANT -package QuickCheck -i"$work"
-iexamples`) and run it. If the source module has top-level value
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
comprehensions. These files are therefore not part of the repository's
`hlint` hygiene line. Generated modules use labeled common/module-specific
sections and are checked separately with `-Wall -Werror` and `hlint`.
