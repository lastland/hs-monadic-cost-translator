# Context

The cost-monadic translator translates a single pure Haskell module into an
instrumented module targeting the Memorist cost-semantics runtime
(`MemoristANT`). The translated module makes sharing and evaluation explicit:
every ordinary datatype field and shared value becomes a named thunk (`Th`),
execution runs inside the cost monad (`MemoristM`), and every source-function
invocation charges one tick. The translator also generates representation
types, demand types, conversion instances, and QuickCheck generators for
property-based cost testing.

The glossary is organized by area. Each area keeps its own ubiquitous language.

## Relationships

- **Pipeline → Validation**: The pipeline invokes semantic gates to reject unsupported source constructs and check scalar comparisons before emitting candidate ASTs.
- **Pipeline → Codegen**: The pipeline coordinates AST emission through codegen builders and invokes the pretty-printer to render candidate source text.
- **Validation → Datatypes & Sharing**: Type evidence recovered during validation informs datatype representations and determines whether value sharing requires type reflection.
- **Datatypes ↔ Sharing**: Representation datatypes wrap fields in explicit thunks managed by sharing combinators; demand generators construct demand trees that mirror thunk structures.
- **Codegen → Datatypes, Sharing, & Validation**: Codegen provides the underlying structural builders (`Cost.Translate.Output`) that construct AST nodes for datatypes, sharing primitives, and validation assertions.

## Pipeline

The compilation lifecycle and orchestration pass that drives GHC API sessions, allocates collision-free variable names, verifies emitted candidate modules with external typechecking, and publishes valid outputs atomically.

### Language

**Translation Lifecycle**:
The sequenced multi-stage process (`Frontend.generate` through candidate emission, external GHC verification, and atomic rename) that converts a pure Haskell source module into an instrumented cost-monadic module.
_Avoid_: Compilation pipeline, build loop

**GHC Session**:
The in-process GHC API execution context configured with `noBackend` and `NoLink` to inspect renamed syntax and extract type evidence without running code generators or optimizers.
_Avoid_: Compiler instance, GHC environment

**Renamed Syntax**:
The GHC abstract syntax tree (`HsGroup GhcRn`) after name resolution, where every identifier is resolved to its defining entity while syntactic structure remains unoptimized.
_Avoid_: Desugared syntax, Core

**Name Plan**:
The pre-allocated table of fresh identifiers computed before AST construction to prevent variable collisions across emitted expressions and helpers.
_Avoid_: Fresh name generator, symbol table

**Candidate**:
The rendered Haskell source text written to a temporary directory for verification before publication.
_Avoid_: Draft output, scratch file

**Verification Gate**:
The external execution of `ghc -fno-code` against a candidate module to verify that it typechecks cleanly against `MemoristANT` and `QuickCheck`.
_Avoid_: Typecheck test, sanity check

**Publication Gate**:
The atomic filesystem rename that moves the verified candidate over the destination path only after external typechecking succeeds.
_Avoid_: Overwrite pass, file commit

**Diagnostic**:
A structured classification distinguishing unsupported source features, invalid module names, environment setup errors, and generated-code type errors.
_Avoid_: Compiler error, warning message

## Datatypes

The domain covering algebraic datatype representations, demand trees, kind-indexed mappings across higher kinds, dictionary witness classes, budget-bounded demand generation, and direct container folding.

### Language

**Representation Type**:
A generated algebraic datatype (distinguished by a `T` suffix) whose constructor fields are wrapped in explicit thunks (`M.Th`) over the field representation types.
_Avoid_: Wrapped type, thunked type, monadic type

**Demand Type**:
A generated algebraic datatype (distinguished by a `D` suffix) whose constructor fields are wrapped in demand thunks (`M.ThD`) to specify structural evaluation depth.
_Avoid_: Pattern type, mask type

**Kind-Indexed Family**:
A closed type family (`RepOf` or `DemOf`) indexed by an explicit kind parameter that maps types and type constructors to their representation or demand counterparts.
_Avoid_: Type function, kind mapping

**Dictionary Witness**:
A generated typeclass (`ReprApp` or `DemandApp`) carrying equality constraints between type applications and their representation forms to resolve quantified constraints in recursive datatypes.
_Avoid_: Evidence class, proof dictionary

**Quantified Constraint Lifting**:
The technique of packaging runtime class obligations across type applications through quantified constraints and constraint synonyms (`ReprK`, `DemandK`, `GeneratedK`, `ShapeK`).
_Avoid_: Higher-order constraints, class wrappers

**Budget-Bounded Demand Generation**:
A QuickCheck generator algorithm (`gen*Demand`) that decrements a finite integer budget across fields and emits `Skip` upon exhaustion to prevent infinite demand trees.
_Avoid_: Sized generation, fuel generation

**Compatible Shrinking**:
A demand shrinker (`shrink*Demand`) that preserves the observed constructor and either replaces demanded fields with `Skip` or recursively shrinks existing sub-demands.
_Avoid_: Standard shrinking, structural reduction

**FoldableT**:
A generated typeclass providing direct, one-tick container folding (`foldrT`, `foldlT`, `concatMapT`) over thunked data structures without converting containers to intermediate lists.
_Avoid_: Monadic Foldable, list adapter

## Sharing

The domain governing explicit thunk allocation, call-by-need preservation, polymorphic and dictionary-keyed thunk reuse, top-level value initialization, and calling convention validation.

### Language

**Call-by-Need Preservation**:
The property ensuring that lazy value bindings and captured expressions evaluate at most once per execution instance, matching Haskell runtime sharing behavior.
_Avoid_: Lazy caching, memoization

**Thunk Allocation**:
The creation of an explicit named thunk (`M.Th`) wrapping a suspended computation, retaining its identity across multiple references.
_Avoid_: Suspension creation, delayed evaluation

**Unconstrained Sharing (`sharedAt`)**:
The allocation of a single runtime thunk at a fixed reserved name, shared across all type instantiations of an unconstrained polymorphic value.
_Avoid_: Monomorphic sharing, static sharing

**Dictionary-Sensitive Sharing (`sharedAtBy`)**:
The allocation of distinct thunks keyed by structural type representations of the generalized variables appearing in dictionary constraints.
_Avoid_: Constrained sharing, class-keyed caching

**Structural Type Key**:
An integer sequence computed by `sharingTypeKey` from `Reflection.SomeTypeRep` that uniquely encodes qualified type constructors, kind arguments, and applications.
_Avoid_: Type hash, string key

**Negative Delimiters**:
Negative integer markers (`-1` and `-2`) inserted into thunk name ancestry to ensure structural type keys remain prefix-free and isolated from ordinary allocator names.
_Avoid_: Namespace separators, key prefixes

**Bindings Record**:
A generated data structure grouping top-level value thunks and functions, created once per evaluation by `initialize :: MemoristM Bindings` to preserve top-level sharing across calls.
_Avoid_: Global environment, state record

**Callable**:
An expression classified as a named function or composition spine, validated against its arity and calling convention to govern adapter emission.
_Avoid_: Function reference, invokable

## Validation

The domain enforcing semantic constraints, inspecting desugared Core for scalar comparisons, restricting comparison operands via the `Scalar` class, and recovering precise type evidence.

### Language

**Semantic Gate**:
A pre-generation check that inspects source declarations, exports, or Core structures and aborts translation with a source-located diagnostic when encountering an unsupported construct.
_Avoid_: Syntax validator, safety filter

**Scalar Comparison Gate**:
The validation pass walking desugared Core (`validateComparisons`) that requires every instantiated `Eq` and `Ord` constraint to operate strictly on scalar types.
_Avoid_: Equality check, Core validator

**Scalar Type**:
A primitive type without internal thunk metadata (`Int`, `Integer`, `Bool`, `Float`, `Double`, `Char`, `()`), safe for direct value-level comparisons.
_Avoid_: Primitive type, unboxed type

**Scalar Class**:
A generated typeclass (`Scalar`) and its closed instance set that restricts polymorphic comparison operands to verified scalar types in generated code.
_Avoid_: Primitive class, comparison witness

**Type Evidence Recovery**:
The targeted desugaring of selected source expressions (`HsVar` and `ExprWithTySig`) to obtain exact instantiated GHC types without using desugared Core for AST emission.
_Avoid_: Type reconstruction, inference extraction

## Codegen

The domain constructing parsed abstract syntax tree nodes directly, managing operator precedence structurally, rendering Haskell code, and maintaining layout invariants.

### Language

**Structural AST Construction**:
The direct creation of `GhcPs` syntax nodes through dedicated combinators (`Cost.Translate.Output`) without parsing source string fragments or invoking lexers.
_Avoid_: String templating, code emission, quotation

**Operator Precedence Management**:
The automatic parenthesization of subexpressions, types, and patterns during AST construction using GHC's internal precedence combinators (`parenthesizeHsExpr`).
_Avoid_: Bracket insertion, precedence fixing

**Pretty-Printing Pass**:
The single-pass rendering of an assembled `HsModule GhcPs` into target Haskell source text via `Cost.Translate.Pretty.prettyModule`.
_Avoid_: Code formatting, printing pipeline

**Extension Tier**:
A graduated set of language extension pragmas (`basicExtensions`, `polymorphicExtensions`, `reflectionExtensions`, `requiredExtensions`) enabled selectively according to module features.
_Avoid_: Extension list, pragma set

**Layout Invariants**:
Formatting rules ensuring that type signatures remain attached to their corresponding bindings, imports are partitioned and sorted, and blank lines separate logical blocks.
_Avoid_: Formatting conventions, indentation rules
