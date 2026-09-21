# Pipeline

The compilation lifecycle and orchestration pass that drives GHC API sessions, allocates collision-free variable names, verifies emitted candidate modules with external typechecking, and publishes valid outputs atomically.

## Language

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
