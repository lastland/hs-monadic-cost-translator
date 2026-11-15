# Haskell Cost-Monadic Translator

The cost-monadic translator translates a single pure Haskell module into an instrumented module targeting the Memorist cost-semantics runtime (`MemoristANT`). The translated module makes sharing and evaluation explicit: every ordinary datatype field and shared value becomes a named thunk (`Th`), execution runs inside the cost monad (`MemoristM`), and every source-function invocation charges one tick. The translator also generates representation types, demand types, conversion instances, and QuickCheck generators for property-based cost testing. The translator operates through the GHC API, maintains zero build-time dependencies on the external runtime, and verifies every generated candidate through an external GHC typechecking pass before publication.

## Installation and Use Guide

### Prerequisites

The translator requires:

- **Stack with pinned resolver**: Stack configured with **LTS 24.22** running **GHC 9.10.3** (specified in `stack.yaml`).
- **External runtime checkout**: A local checkout of the `MemoristANT` runtime package placed at `hs-pbt/` in the repository root. This path is git-ignored; the repository neither bundles nor clones it automatically.
- **QuickCheck dependency**: `QuickCheck-2.17.1.0`, pinned as an `extra-dep` in `stack.yaml` to match the external runtime's dependencies.

The root `stack.yaml` registers both `.` and `hs-pbt/` as packages so that GHC environments invoked under `stack run` and `stack test` can locate `MemoristANT` and `QuickCheck`.

### Compiling and Running the Translator

Build the project with Stack:

```sh
stack build
```

Translate a source module by supplying the source path and the desired output path:

```sh
stack run cost-translate -- SOURCE.hs OUTPUT.hs
```

The command-line interface accepts exactly two arguments. The output filename must specify an uppercase Haskell module name (such as `Target.hs`, producing `module Target where`). If the filename is lowercase or malformed, the translator aborts with a diagnostic before generating code. Ready-to-try source files reside in [`examples/`](examples/), and [`examples/README.md`](examples/README.md) provides per-file walkthroughs with verified values and costs.

The translator writes a companion log file, `OUTPUT.hs.log`, next to the output path. The log records source-located function identities, syntactic arities, supplied argument counts, adapter selections, and compiler diagnostics. If translation fails, the translator appends the failure reason to the log.

To prevent incomplete or invalid outputs, the translator compiles candidates in isolation. It writes candidate source text to a temporary directory (`.cost-translate-XXXXXX/`) and typechecks that file with an external GHC command:

```sh
ghc -v0 -fno-code -fforce-recomp -i<sourceDir> -package MemoristANT -package QuickCheck candidate.hs
```

If typechecking succeeds, the translator atomically renames the candidate to `OUTPUT.hs`. If typechecking fails, the translator appends GHC's error messages and the rejected candidate source text to `OUTPUT.hs.log` and exits with an error. The destination file `OUTPUT.hs` remains untouched, ensuring that a failed translation never clobbers a previously valid output. Always inspect the exit status and log file after running the tool.

### Using the Generated Code

The generated module imports the original source module qualified as `Source` (`import qualified <SourceModule> as Source`) to construct conversion instances between pure source types and monadic representation types. Downstream builds must therefore keep both modules accessible on GHC's search path.

Compile client programs with `ghc` (using the pinned GHC 9.10.3 compiler), exposing `MemoristANT`, `QuickCheck`, and the directory containing the source module (`-i<sourceDir>`).

Handwritten driver programs test generated functions against the pure source and verify demanded costs using the runtime's verification combinators:

- `correct :: MemoristM (Rep a) -> a -> Bool`: verifies that the monadic computation yields a value structurally equivalent to the pure source result.
- `infcost :: MemoristM a -> (a -> MemoristM (Dem a)) -> Cost`: evaluates the computation under a structural demand function and returns the total accrued ticks. Passing `fullDemand` forces the result completely.
- `fromPure1`, `fromPure2`, etc.: adapt pure arguments into the monadic calling conventions expected by translated functions.

The following driver verifies value correctness and measures exact tick costs for a generated function `doubleM`:

```haskell
module Main where

import Generated
import qualified Double as Source
import Memorist.Memorist hiding (toInteger)
import qualified Memorist.Base.Thunk as MT

main :: IO ()
main = do
    -- Verify value correctness against pure source
    print (correct (fromPure1 doubleM (5 :: Int)) (10 :: Int))

    -- Measure exact ticks under full demand
    let cost = infcost (fromPure1 doubleM (5 :: Int)) fullDemand
    print (MT.toInteger cost)
```

If the source module defines zero-arity top-level value bindings (such as `seed = double 5`), the generated module cannot export them as pure constants because thunk allocation requires monadic effects. Instead, the translator emits a `Bindings` record containing all initialized thunks and functions, alongside an initialization action:

```haskell
initialize :: MemoristM Bindings
```

A client driver executes `initialize` once and reuses the resulting record fields across subsequent calls to preserve sharing across distinct computations:

```haskell
do bindings <- initialize
   a <- fromPure1 (firstM bindings) (5 :: Int)
   b <- fromPure1 (secondM bindings) (5 :: Int)
   pure (a + b)
```

Reusing the record preserves top-level thunk identity, ensuring that shared thunks evaluate at most once across separate function calls.

### Running the Test Suite

The repository contains two test suites:

- **Root test suite**: Run `stack test` to execute `cost-translate-test` (`test/Main.hs`). This suite tests whole-module translation across the supported language subset, validates CLI failure modes, checks arity logging, and verifies that generated modules typecheck and evaluate to expected values and costs.
  
  To run specific test cases, pass test names through `--test-arguments`:

  ```sh
   stack test cost-translate:test:cost-translate-test --test-arguments="scanl-unused-input"
   stack test cost-translate:test:cost-translate-test --test-arguments="diagnostics explicit-constructor-exports"
   stack test cost-translate:test:cost-translate-test --test-arguments="thunk-case thunk-guards"
   stack test cost-translate:test:cost-translate-test --test-arguments="anf-demand thunk-matching-sharing anf-properties"
   ```

  The thunk tests check runtime reference identity and fresh-name progression as
  well as values, laziness, and entry ticks. Test names are exact matches; check
  the `PASS` output to confirm that the requested groups ran.

  `anf-demand` checks computed suspensions, explicit source-binding anchors,
  pattern/guard order, and scalar/Boolean demand. `thunk-matching-sharing` checks
  polymorphic and dictionary-sensitive references through matching, including
  scoped and defaulted uses. `anf-properties` translates its fixture once and
  checks all four pair-demand masks with bounded inputs, repeated demands, and
  multi-output captures/compositions. It runs both seeded and randomized cases;
  failures retain shrunk counterexamples and seed information in the driver log.

- **Integration test suite**: Run `stack --stack-yaml integration/stack.yaml test` to execute the dedicated integration project (`integration/cost-translate-integration.cabal`). This project tests generated modules and handwritten probes directly against `MemoristANT` and QuickCheck:
  - `cost-translate-integration`: runs Tasty, HUnit, and QuickCheck test suites (P1–P7) verifying exact tick formulas, value equivalence, and composition chains.
  - `runtime-scanl`: verifies the demand-laziness regression fix for `scanlM` (delaying input-spine and callback forcing).
  - `runtime-polymorphic-sharing`: verifies that unconstrained polymorphic thunks share a single runtime name across distinct type instantiations.
  - `runtime-higher-kinded`: verifies higher-kinded datatype conversions, demand generation, and compatible shrinks.

To check code style and lint rules across both projects, run:

```sh
hlint src app test integration/probes integration/test-integration
```

## Features

### Pipeline and Compilation Model

The translation pipeline converts a pure Haskell source module into a verified, instrumented module through distinct stages:

1. **GHC API session**: `Cost.Translate.Frontend.generate` configures an in-process GHC session with `backend = noBackend` and `ghcLink = NoLink`. GHC parses, renames, and typechecks the source module without running the optimizer, simplifier, or linker. This preserves the original source structure and prevents optimizations from shifting tick boundaries.
2. **Renamed syntax instrumentation**: The translator inspects and transforms the renamed syntax tree (`HsGroup GhcRn`). In this tree, GHC has resolved all unqualified names, constructor references, and variable bindings to unambiguous global or local identities (`Name`), while preserving the original pattern matching, guards, and call sites.
3. **Targeted desugaring for type evidence**: The translator never emits code from GHC's desugared Core. However, emitting accurate signatures and typeclass dictionaries requires exact types at variable occurrences. `Cost.Translate.Typed.expressionTypes` runs a targeted desugaring pass (`initDs` and `dsLExpr`) solely on selected `HsVar` and `ExprWithTySig` nodes from `tm_typechecked_source`. It reads `Core.Utils.exprType` to obtain the precise instantiated type of each expression, recovering dictionary evidence without re-implementing GHC's type inference rules.
4. **Candidate generation and NamePlan**: Before constructing syntax trees, a pre-pass computes a `NamePlan`. The plan pre-allocates collision-free variable names for parameters, continuation actions, and thunk anchors across all bindings. Emitters in `Cost.Translate.Output` construct abstract syntax trees directly as `GhcPs` nodes. Combinators automatically manage operator precedence and parenthesization (`parenthesizeHsExpr`), eliminating string concatenation and reparsing.
5. **External typechecking gate**: `Cost.Translate.Pretty.prettyModule` formats the completed `HsModule GhcPs` into target text in a single pretty-printing pass. `Cost.Translate.translateFile` writes this text to a temporary candidate file and invokes an external `ghc` process with `-fno-code -package MemoristANT -package QuickCheck`. Only when the external compiler confirms that the candidate typechecks does the translator rename the file to the final destination path.

### Cost Policy

The translator enforces a precise cost model aligned with the source-level semantics of the Memorist paper:

- **1 tick per source function call**: Every translated function executes `M.tick` immediately upon entry. This rule applies uniformly to top-level functions, local helper functions, and trivial wrappers.
- **0 ticks for constructor allocation**: Constructing data values (such as `Just x`, `x : xs`, or `Tree l r`) allocates value cells in `MemoristM` without emitting ticks.
- **0 ticks for scalar primitives**: Primitive arithmetic operators (`+`, `-`, `*`, `div`), relational operators (`==`, `/=`, `<`, `<=`, `>`, `>=`), and short-circuit booleans (`&&`, `||`) add no entry ticks. If their operands evaluate computations, those operands contribute ticks when forced.
- **0 ticks for composition and partial-application adapters**: Composing functions with `(.)` or partially applying arguments creates unticked functional adapters. Only the underlying functions charge ticks when called.
- **0 ticks for conversion methods**: Datatype conversions (`reprM`, `abstract`, and `fullDemand`) convert between source values and thunked representations without charging ticks.
- **1 tick for generated record selectors**: Generated record field selectors act as single-argument accessor functions and charge one entry tick.
- **1 tick for direct container folds**: Direct `FoldableT` methods (`foldrT`, `foldlT` on `MaybeT` and `PairT`, and `concatMapT`) charge one entry tick, retaining any ticks accrued by the supplied callback.

### Supported Language Subset

The translator supports a rich subset of pure Haskell:

- **Data declarations**: Algebraic datatypes with multiple constructors, nullary constructors, parameterized types, recursive datatypes, and nested recursion (such as `data Nested a = End a | More (Nested (a, a))`). It also supports higher-kinded parameters (e.g., `data Higher f a = Higher (f a)`), recursive constructor parameters (`data Fix f = In (f (Fix f))`), infix constructors (e.g., `a :*: b`), and record declarations with named fields.
- **Pattern matching**: Ordered equations, variable patterns, wildcard patterns (`_`), constructor patterns, list patterns (`[]`, `:`), tuple patterns, and as-patterns (`x@pat`). Literal patterns include integers, floating-point numbers, characters (`'a'`), and strings (`"abc"`). String literal patterns expand into character-by-character matches over the thunked list representation, stopping at the first mismatch. Record patterns extract fields by declaration position while matching supplied subpatterns in source order.
- **Guards**: Boolean guards, pattern guards (`pat <- expr`), `let` guards (`let x = expr`), and comma-separated conjunctive guards. Branch and equation failure continuations are let-bound monadic actions (`shareAction`) that execute only when a pattern or guard fails, ensuring that skipped branches add neither thunk allocations nor ticks.
- **Local bindings and control flow**: `let` and `where` expressions, named local functions, mutually recursive local functions, captured lexical variables, and non-recursive value bindings. Supported control flow includes `if-then-else`, `case-of`, suspended `undefined`, literal-message `error`, scalar arithmetic, and comparisons over strict scalars.

### Library Replacements

The translator maps common Prelude functions and selected runtime helpers directly to instrumented implementations provided by `MemoristANT`:

| Source operation | External runtime replacement | Description |
| :--- | :--- | :--- |
| `map` | `Memorist.Example.List.mapMM` | Instrumented list map |
| `foldr`, `foldl` | Direct `FoldableT.foldrT` / `foldlT` | Direct container fold; list instances delegate to `foldrM` / `foldlM` |
| `concatMap` | `FoldableT.concatMapT` | Charges 1 entry tick; delegates list instances to `concatMapM` |
| `filter` | `Memorist.Example.List.filterM` | Instrumented list filter |
| `zipWith` | `Memorist.Example.List.zipWithM` | Instrumented list zipWith |
| `scanl` | `Memorist.Example.ListInf.scanlM` | Instrumented scanl using the corrected demand-lazy implementation |
| `head`, `tail` | `Memorist.Example.List.headM`, `tailM` | Instrumented list head and tail |
| `repeatly` | `Memorist.Example.NewtonRaphson.repeatlyM` | Infinite producer with unticked callback adapter |
| `(.)` | Generated unticked composition adapter | Evaluates captured arguments once and specializes on arrival of the first argument |

The translator selects replacements strictly by resolved entity identity (`GHC.Types.Name`). If a source module defines its own function named `map` or `foldr`, the translator translates the local definition normally rather than replacing it. Supported external imports are restricted to `Prelude` and `Memorist.Example.NewtonRaphson` (for `repeatly` only).

### Datatypes and Demands

For every source datatype, the translator generates representation types, demand types, conversion instances, and QuickCheck generators:

- **Representation types (`T` suffix)**: For each source datatype `Tree a`, the translator generates `data TreeT a = ...` where every constructor field is wrapped in a named thunk `M.Th (Rep a)`.
- **Demand types (`D` suffix)**: The translator generates `data TreeD a = ...` where every constructor field is wrapped in a demand thunk `M.ThD (Dem a)`.
- **Typeclass instances**: The translator emits `Repr (Tree a)` (with `Rep`, `reprM`, and `abstract`), `Demand (Tree a)` (with `Dem` and `fullDemand`), `LBoundDemand`, `DemandArb`, and structural representation-demand marker instances.
- **Demand generation and compatible shrinking**: The translator generates `genTreeDemand` and `shrinkTreeDemand` helpers for property testing:
  ```haskell
  genTreeDemand    :: GeneratedDemand a => Int -> TreeT a -> Gen (TreeD (Dem a))
  shrinkTreeDemand :: GeneratedDemand a => TreeT a -> TreeD (Dem a) -> [TreeD (Dem a)]
  ```
  The generator inspects the observed constructor of the runtime representation value and distributes a finite integer budget (derived from QuickCheck's `size`) across the fields. When the budget reaches zero, it emits `M.Skip`. When positive, it chooses between `M.Skip` and `M.Need` with a 1:3 ratio. Skipped fields leave the underlying thunks unforced (`M.Skip`), ensuring tests never evaluate undemanded components. Shrinkers preserve the observed constructor, either replacing demanded fields with `M.Skip` or recursively shrinking sub-demands. QuickCheck properties test results using `forAllShrink`:
  ```haskell
  forAllShrink (genTreeDemand 20 result) (shrinkTreeDemand result) property
  ```

### Higher-Kinded Datatypes

Datatypes with higher-kinded parameters (e.g. `data Higher f a = Higher (f a)`, `data Fix f = In (f (Fix f))`, `data Mixed f a = Leaf a | Branch (f (Mixed f [a]))`) require kind-aware mappings:

- **Kind-indexed type families**: The translator generates closed type families indexed by kind, annotated with standalone kind signatures (`StandaloneKindSignatures`):
  - `RepOf (k :: Type) (a :: k) :: RepOfKind k`
  - `DemOf (k :: Type) (a :: k) :: DemOfKind k`
  At kind `Type`, `RepOf` and `DemOf` map to runtime families `Rep` and `Dem`. At constructor kinds (`Type -> Type`), they map standard containers (`[]` to `ListT`, `Maybe` to `MaybeT`, `(,)` to `PairT`) and user datatypes (`Tree` to `TreeT`). A recursive equation resolves type applications:
  ```haskell
  RepOf (k1 -> k2) (f a) = (RepOf (k1 -> k2) f) (RepOf k1 a)
  ```
- **Quantified constraints and witness classes**: `ReprK` and `DemandK` constraint synonyms lift runtime representation and demand contracts across type applications. Explicit witness classes `ReprApp` and `DemandApp` provide equality evidence (`Rep (f a) ~ RepOf (k1 -> k2) f (RepOf k1 a)`) required by GHC to resolve dictionaries in recursive structures. `GeneratedK` and `GeneratedReq` lift budget-bounded demand generation across kinds, while `ShapeK` and `ShapeReq` supply `Eq`, `Show`, and `Arbitrary` dictionaries for demand values across kinds.

### Top-Level and Polymorphic Sharing

In Haskell, shared value bindings evaluate once on first access. The translator preserves this call-by-need behavior using explicit thunks (`Th`):

- **Thunk reference reuse**: When an argument, constructor field, case subject, or pattern-guard subject refers to an existing thunk, the translator passes that thunk directly, including through parentheses and type annotations. Variable and wildcard patterns do not force it; constructor and literal patterns inspect it only when reached. Other expressions receive a fresh suspension so that evaluation remains demand-driven. Reuse avoids forwarding allocations and bookkeeping; it does not remove explicit source bindings or source-function entry ticks.
- **Unconstrained polymorphic sharing (`sharedAt`)**: When a value binding has polymorphic type without typeclass constraints (such as `empty :: [a]`), the translator allocates a single thunk at a fixed reserved name using `sharedAt`. Invocations across distinct types (e.g. `[Int]` and `[Bool]`) share the exact same runtime thunk.
- **Dictionary-constrained sharing (`sharedAtBy`)**: When a polymorphic value carries typeclass constraints (such as `shared :: (Num a, Eq a) => (a, [b])`), the computed value depends on the dictionary for `a`, but not on the unconstrained variable `b`. The translator identifies variables occurring in dictionary constraints (`constrainedVariables`). It computes a type-level path through the result type (`sharingPaths`) to project those variables using closed type families (`ShareProject`, `ShareKeys`). `sharedAtBy` queries `Type.Reflection.SomeTypeRep` for the projected dictionary types, converting them into a structural integer list key via `sharingTypeKey`.
- **Negative delimiter names**: Allocator thunks use non-negative integer indices. To isolate structural type keys from ordinary thunk identifiers and prevent collisions between nested shared values, `sharedAtBy` encloses keys with negative delimiters `-1` and `-2`. This guarantees prefix-free thunk naming trees across all dictionary instantiations. Consequently, calls at `(Int, Bool)` and `(Int, Char)` share the same thunk, while calls at `(Integer, Bool)` allocate an independent thunk.

#### Scoped annotations: the exception to alias avoidance

For a reused case or pattern-guard subject, the translator retains a plain `let`
when its type annotation contains free target type variables. The binding keeps
the annotation in its original scope: a nested `forall a` must not capture an
outer function's `a`. For example, inside `fM :: forall a. M.Th a -> M.MemoristM Int`,
the relevant generated bindings can be:

```haskell
let caseThunk0 = (x :: M.Th a)  -- fM's a, before g's signature
    g :: forall a. M.Th a -> M.MemoristM Int
    g _ = do
      M.tick
      ignoreM caseThunk0
```

`caseThunk0` names the existing thunk; it is not `M.lazy (M.force x)`. The binding
preserves `tname`, adds no Memorist thunk allocation, force, or tick, and leaves
unused failures suspended. Keeping the annotation also preserves dictionary
specialization rather than allowing a factory to default to another type.
Ordinary references and closed annotations stay inline; computed subjects still
receive suspensions. This exception does not remove explicit source-binding
anchors. The precise rule is recorded in
[`CONVENTIONS.md`](.scratch/translator-poc/CONVENTIONS.md).

### Repository Layout and Key Modules

The repository organizes its implementation, CLI, tests, and documentation as follows:

| Path | Purpose |
| :--- | :--- |
| `app/Main.hs` | CLI entry point; validates the two-argument command line and invokes translation. |
| `src/Cost/Translate.hs` | Translation coordinator; generates candidate code, writes logs, executes the external GHC verification pass in a temporary directory, and atomically publishes outputs. |
| `src/Cost/Translate/Frontend.hs` | GHC session driver; configures compiler session flags (`noBackend`, `NoLink`), coordinates parsing, typechecking, validation gates, AST emission, and `Bindings` generation. |
| `src/Cost/Translate/Output.hs` | Structural `GhcPs` AST builder; constructs expressions, types, patterns, and declarations with automatic precedence management. |
| `src/Cost/Translate/Pretty.hs` | Single-pass pretty printer; formats module preambles, language pragmas, sorted imports, and section banners. |
| `src/Cost/Translate/Names.hs` | Injective naming scheme; mangles identifiers with `M`, `T`, `D` suffixes and encodes operators into decimal representations. |
| `src/Cost/Translate/Datatypes.hs` | Emits representation types, demand types, conversion instances, and QuickCheck demand generators. |
| `src/Cost/Translate/Kinds.hs` | Generates kind-indexed type families (`RepOf`/`DemOf`), kind signatures, and quantified witness classes (`ReprApp`, `DemandApp`). |
| `src/Cost/Translate/Sharing.hs` | Implements call-by-need thunk allocation: `sharedAt` for unconstrained values and `sharedAtBy` with negative delimiters for dictionary-keyed values. |
| `src/Cost/Translate/Callable.hs` | Validates function application and composition spines, checks arities, and constructs unticked adapters. |
| `src/Cost/Translate/Foldable.hs` | Emits `FoldableT` and instances for lists, `MaybeT`, and `PairT`. |
| `src/Cost/Translate/Validation.hs` | Traverses desugared Core to enforce the scalar comparison gate; emits the `Scalar` class and instances. |
| `src/Cost/Translate/Typed.hs` | Runs targeted desugaring to recover GHC inferred types for variable expressions and annotations. |
| `src/Cost/Translate/Diagnostic.hs` | Defines phase-preserving diagnostic types (`UnsupportedSource`, `GeneratedModuleNameInvalid`). |
| `test/Main.hs` | Root end-to-end test suite (`cost-translate-test`). |
| `test/RuntimeScanlCandidate.hs` | External runtime regression gate for demand-lazy `scanlM`. |
| `test/RuntimePolymorphicSharingGate.hs` | External runtime regression gate for polymorphic thunk sharing. |
| `test/RuntimeHigherKindedGate.hs` | External runtime regression gate for higher-kinded datatype conversions and demand generation. |
| `integration/` | Independent Stack project verifying generated code against `MemoristANT` and QuickCheck (`cost-translate-integration.cabal`, `test-integration/Main.hs`, `probes/Probes.hs`). |
| `docs/adr/` | Architecture decision records (0001–0004) documenting cost semantics, GHC API boundaries, external runtime interface, and explicit thunk sharing. |
| `examples/` | Standalone source examples with step-by-step walkthroughs and verified values and costs. |

### Bridge to the Memorist Paper

The generated code directly reflects the operational semantics defined in the Memorist paper:

| Paper Concept | Codebase Realization | Description |
| :--- | :--- | :--- |
| `MemoristM` | `Memorist.Memorist.MemoristM` | Cost monad tracking ticks and allocation state |
| `Th a` | `Memorist.Base.Thunk.Th` | Explicit named thunk wrapping a suspended computation of type `a` |
| `lazy` | `Memorist.Memorist.lazy` | Monadic action suspending a computation into a `Th` |
| `eval` / `force` | `Memorist.Memorist.eval` / `force` | Forces a thunk, records ticks to execution trace, and caches the result |
| `tick` | `Memorist.Memorist.tick` | Cost unit incrementing the execution cost counter |
| `Rep a` / `Dem a` | `Repr` / `Demand` type families | Open type families associating source types with representation and demand types |
| `fullDemand` | `fullDemand` | Structural demand function forcing every constructor and field |
| `infcost` | `Memorist.Memorist.infcost` | Evaluates a computation under a demand function, returning total ticks |
| `correct` | `Memorist.Memorist.correct` | Verifies value equivalence between monadic computation and pure source value |
| `fromPure1`, `fromPure2` | `Memorist.Memorist.fromPure1` etc. | Adapts pure function arguments into monadic call conventions |

In translating theoretical semantics into a working compiler, the implementation addresses three concrete realities:

1. **Char identity instances**: The external `MemoristANT` package lacks `Repr` and `Demand` instances for `Char`. To avoid modifying the external runtime, each generated module emits local identity conversions (`Rep Char = Char`, `Dem Char = ()`), treating `Char` as a full scalar primitive.
2. **`Bindings` record**: In the idealized paper, global values evaluate lazily in a persistent top-level heap. In compiled Haskell, top-level values cannot perform monadic side effects on module loading. The translator resolves this by packaging top-level zero-arity bindings into a `Bindings` record produced by `initialize :: MemoristM Bindings`, enabling test drivers to allocate and share top-level thunks across calls.
3. **`FoldableT`**: Rather than translating general source typeclasses or converting containers to intermediate lists (which would distort tick costs), the translator provides a specialized `FoldableT` class with direct, one-tick pattern-matching implementations for `MaybeT` and `PairT`.

## Limitations

### Excluded Constructs

The translator rejects the following constructs, reporting source-located diagnostics:

- Anonymous lambdas (`\x -> ...`).
- `newtype` declarations.
- User-defined type classes and instances.
- `deriving` clauses on data declarations.
- Strict field annotations (`!a`), strict patterns, and `Strict`/`StrictData` language pragmas.
- Recursive value bindings (e.g., `let xs = 1 : xs`).
- Multi-module source trees or user-module imports (only `Prelude` and `Memorist.Example.NewtonRaphson` are permitted).
- Higher-rank types, polymorphic recursion, record updates, and list comprehensions.

### Semantic Constraints

- **Explicit constructor exports**: Generated conversion instances (`reprM`, `abstract`) must construct and pattern-match on source values. When a source module specifies an explicit export list, all constructors of datatypes used in generated conversions must be exported (e.g. `data Box = Box Int` requires `Box(..)`). If a constructor is hidden, the translator halts with a diagnostic before generating code.
- **Comparisons restricted to scalar types**: Runtime `Eq` and `Ord` instances on thunked structures compare thunk metadata rather than structural elements. To prevent misleading equality checks, the translator inspects desugared Core and restricts comparison operands to verified scalar types (`Int`, `Integer`, `Float`, `Double`, `Bool`, `Char`, `()`). Structural comparisons on lists or custom datatypes are rejected.

### Environment Constraints

- **Single-module boundary**: The translator processes exactly one source module at a time and does not traverse project dependencies.
- **Local runtime requirement**: The translator does not bundle `MemoristANT`. Verification and compilation require an external GHC environment where `MemoristANT` and `QuickCheck` are available (provided via `hs-pbt/`).

## AI Usage Declaration

This repository's code, tests, and documentation were developed with assistance from generative AI tools within the OpenCode agent environment, under human direction and review. All code and documentation were compiled and verified against the repository's automated test suites.
