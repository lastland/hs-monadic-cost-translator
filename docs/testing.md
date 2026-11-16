# Testing and Reference-Comparison Guide

The root test suite (`cost-translate-test`, `test/Main.hs`) verifies every successful-translation fixture by comparing the translated module against an independent, handwritten cost-monadic reference. The runner enforces this architecture in code—not through prompt conventions—via a mandatory static policy gate (`test/ReferencePolicy.hs`) and a single shared execution pipeline (`test/ReferenceRunner.hs` and `test/reference/`).

## Compiler Selection and Runtime-Free Validation

Only **GHC 9.10.3** is supported. The Cabal `ghc` and `ghc-boot-th` dependency
bounds reject other versions before compilation; `tested-with` records the pin.

Run `sh scripts/validate.sh` for the runtime-free CI build and checks with
`-Wall -Werror`. It discovers an installed `ghc-9.10.3`, a matching `ghc` on
`PATH`, or Stack's installed compiler, in that order. It never installs GHC.
Stack discovery uses a compiler-only configuration with no runtime packages.
Set `GHC=/path/to/ghc` to override discovery, or pass `--check-compiler` to run
only the preflight. Validation selects matching `ghc` and `ghc-pkg` for Cabal
and child processes. It requires neither Stack nor `hs-pbt/` when the compiler
is available directly. Run `python3 test/compiler_preflight.py` to check selection
and early rejection without a real compiler. The full semantic and integration
suites still use the Stack commands below.

## Execution Pipeline and Sandbox Isolation

When `cost-translate-test` starts, `test/Main.hs` discovers all semantic fixtures (`discoverReferenceCases`) into a typed `Group` registry with no arbitrary `IO` callbacks and runs `validateReferenceCases` across the entire repository **before** executing any selected group. Even a single-case run fails immediately if any fixture directory, `Case.hs`, `Reference.hs`, or `.cabal` test-suite stanza violates policy.

For each semantic fixture, `ReferenceRunner` creates two isolated sandboxes under a temporary run directory (`generated/` and `reference/`):

1. Both sandboxes receive the pure source fixture (`Example.hs`, or a registered alias from `sourceAliases` in `test/ReferencePolicy.hs`) and the shared `Case.hs`.
2. In `generated/`, `Cost.Translate.translateFile` translates the source module into `Generated.hs`. In `reference/`, `ReferenceRunner` copies the handwritten `Reference.hs` to `Generated.hs`.
3. `ReferenceRunner` compiles `test/reference/Observe.hs` against `Case.hs` and `Generated.hs` in each sandbox using `ghc -dynamic -Werror=unused-imports -package MemoristANT -package QuickCheck`.
4. The shared controller (`test/reference/Compare.hs`) starts both `observe` binaries as line-buffered child processes over `stdin`/`stdout` (`test/reference/Protocol.hs`), compares them across fixed boundary `Sample` inputs and 200 seeded QuickCheck trials with shrinking, and writes `comparison.log`.
5. After comparison passes, `test/GeneratedChecks.hs` runs any applicable post-comparison artifact checks on `generated/` (`-Wall -Werror` and HLint on `generated-warnings*`, code-size bounds on `pattern-scaling`, maximal-application arity logs on `foldr-arity-four-partial`, single-inspection constructor grouping on `ordered-equations-and-conjunctive-guards`, and terminal ordinary-`do` flattening on `anf-demand`). The paired binder pre-check on `anf-scoped-renaming` runs before comparison.

Compiling the two workers in separate sandboxes ensures that both sides can declare `module Generated` and define representation types (`*T`), demand types (`*D`), and typeclass instances (`Repr`, `Demand`, `FoldableT`) without symbol or orphan-instance collisions, while still executing the exact same `Case.hs`. Dynamic linking (`-dynamic`) keeps temporary executable disk usage modest and leaves the root library and `cost-translate-test` executable free of `MemoristANT` and `QuickCheck` Cabal build dependencies.

## Manual Regression Examples

The five `manual-*` cases use the actual sources in [`examples/`](../examples/README.md),
not fixture copies. `ReferencePolicy.manualExamples` registers each example source
with its selector. The mandatory inventory gate rejects missing examples,
missing reference cases, source substitutions, and missing source/selector rows
in `examples/README.md`. Each row must name the source file and selector on the
same line. Its negative self-tests run under `fixture-policy` without the target
runtime.

Source paths returned by discovery are Cabal data-file paths. `ReferenceRunner`
stages each source under its declared filename as well as `Example.hs`, retaining
support for the original fixture aliases. The policy permits each manual case
to import only its registered source module. Cabal packages the five explicit
source paths; it does not collect generated outputs from `examples/`.

After semantic comparison, these five maintained sources must compile with
`-Wall -Werror` and pass HLint; HLint must be available. `ScopedAnnotations.hs`
suppresses the intentional type-variable-shadowing warning, and inline HLint
exceptions preserve the regression syntax. The generated-output warning checks
remain bounded to `generated-warnings*`; the new constructor and terminal-`do`
examples also run their respective structural AST checks. All five shared
reference-support modules enable fatal warnings per module on every semantic
run, without extending warning flags to the generated implementations.

Existing static fixtures deliberately exercise partial `head`/`tail` calls and
redundant guard equations. Their `-Wx-partial` and `-Woverlapping-patterns`
warnings remain fixture exceptions; the maintained runner/support modules and
the five new source examples do not share this exception.

## Probe Catalog API (`ReferenceSupport`)

`Case.hs` exports a static catalog `probes :: [Probe]` built from constructors in `test/reference/ReferenceSupport.hs` (see `test/reference/Protocol.hs` for `Sample` fields and `test/reference/Compare.hs` for boundary inputs and seeds):

- **`scalar`, `boolean`, `integer`, and `demanded`**: Evaluate a `MemoristM` computation, force the observed scalar or finite projection, and compute exact demanded ticks (`M.calcCost`). Unexpected runtime exceptions in the computation or projection hard-fail the worker (`Raised` is rejected).
- **`scalarSource`**: Evaluates an `Int` computation under `M.fullDemand`, compares value and ticks between the two workers, and checks that the result matches the pure `Example` oracle. It accepts `Raised` only when both the monadic computation and the pure source oracle raise an exception.
- **`failure`**: Requires the computation to raise a synchronous exception under `M.fullDemand`, recording `Raised` with no invented tick cost. Normal return values fail the probe, and worker timeouts always abort rather than matching as `Raised`.
- **`snapshot`**: Evaluates a pure structural observation without ticks—such as normalized thunk-alias relationships (`M.tname a == M.tname b`) or caller-level fresh-name progression (`snd (M.runM program M.nzero)`). Exceptions in a snapshot hard-fail the worker.
- **`generatedDemands` (`DemandSupport`)**: Only the generated worker draws demands and shrinks. The controller replays those exact finite plans in both workers and checks actual values, costs, codec round-trips, constructor compatibility, budget bounds, and shrinking. Zero budgets, repeated budget-15 draws, and budgets through 25 are mandatory; the reference never needs to reproduce the generator's random choices.

Every `Case.hs` must define `probes :: [Probe]` as a static catalog that does not take `Sample` as an argument, preventing per-input probe omission. A catalog may use bounded static helpers and list combinators, must use unique non-empty labels, and must include a costed computation or generated-demand probe; empty, `snapshot`-only, and `failure`-only catalogs are rejected. Every costed callback must have syntactic reachability to a generated function or a generated-instance conversion. Datatype-only cases can test conversion instances without adding an artificial source function.

## Adding a Semantic Test Case

Create `Example.hs`, `Reference.hs`, and `Case.hs` under `test/cases/<case-name>/` (as in `test/cases/double/`):

```haskell
-- test/cases/double/Example.hs
module Example where
double :: Int -> Int
double x = x + x
```

```haskell
-- test/cases/double/Reference.hs
module Generated where

import qualified Memorist.Memorist as M

doubleM :: Num a => M.Th a -> M.MemoristM a
doubleM x = do
    M.tick
    a <- M.force x
    b <- M.force x
    pure (a + b)
```

```haskell
-- test/cases/double/Case.hs
module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "double" (Source.double . number) (\s -> M.fromPure1 doubleM (number s))
    , scalar "double-other" (\s -> M.fromPure1 doubleM (other s))
    , scalar "shared-computed-argument" $ \s -> do
        x <- M.lazy (M.fromPure1 doubleM (number s))
        doubleM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 doubleM (undefined :: Int))
    ]
```

Write `Reference.hs` by hand following the cost and thunk rules in
[`docs/adr/0001-source-cost-semantics.md`](adr/0001-source-cost-semantics.md) and
[`docs/adr/0004-explicit-thunks-and-limited-function-translation.md`](adr/0004-explicit-thunks-and-limited-function-translation.md)—never
by copying generated translator output. `Reference.hs` may import `Example` qualified solely to mention
source types and constructors in handwritten `Repr` instances; calling pure source functions inside
`Reference.hs` is forbidden.

Validate policy and run the new case:

```sh
stack test cost-translate:test:cost-translate-test --test-arguments="fixture-policy"
stack test cost-translate:test:cost-translate-test --test-arguments="double"
```

When a case fails, inspect the retained temporary directory printed in the test output
(`Reference fixtures and logs: ...`), which contains `comparison.log`, `generated/Generated.hs`,
`generated/Generated.hs.log`, `generated/compile.log`, and `reference/compile.log`.

## Policy Gates, Exceptions, and Verification Boundaries

`test/ReferencePolicy.hs` and `test/FixturePolicy.hs` run in runtime-free CI under the `fixture-policy`
selector (see those modules for exact AST and manifest rules):

- **Fail-closed inventory and manifest check**: Every directory in `test/cases/` must be a complete
  semantic case (`Example.hs` or registered alias, `Case.hs`, `Reference.hs`) or an explicitly
  registered entry in `ReferencePolicy.hs` for diagnostic rejection (`diagnostics-*`,
  `whole-module-failure`), shared source providers (`sourceOnlyDirectories`), or `fixture-policy-*`
  specimens. Custom `Main.hs`, `Regression.hs`, extra local `.hs` modules, symlinks, and unregistered
  `.cabal` test-suite stanzas or `-main-is` flags are rejected.
- **GHC-parsed module policy**: `ReferencePolicy` parses every `Case.hs` and `Reference.hs` without
  loading plugins or preprocessors, enforcing module headers (`Case` and `Generated`), whitelisted
  pragmas and imports, the ban on source-function value calls in `Reference.hs`, the ban on
  `IO`/`main`/`Property`/QuickCheck trial knobs, and the static `probes :: [Probe]` contract.
  `FixturePolicy` checks that runner file writes in `test/Main.hs` target literal `.log` paths
  (exempting `test/OutputAstProbe.hs`).
- **Harness fault-injection self-tests (`reference-harness`)**: `checkReferenceHarness` in
  `test/ReferenceRunner.hs` verifies that the live `Compare` and `Observe` binaries accept `Matching.hs`
  and reject value, tick, lost-alias, extra-allocation, and premature-force mutants, as well as empty,
  snapshot-only, failure-only, broken-projection, malformed-observer, and wrong-executable faults in
  `test/reference-selftest/`.
- **Supplemental external-runtime suites (`integration/`)**:
  `stack --stack-yaml integration/stack.yaml test` runs external-runtime environment and feasibility
  probes (`cost-translate-integration`, `runtime-scanl`, `runtime-polymorphic-sharing`,
  `runtime-higher-kinded`). These test the `MemoristANT` runtime environment rather than replacing
  semantic reference comparisons in `cost-translate-test`.

**Guarantees vs. human review**: The static gate enforces fixture, catalog, dependency, and registration
structure. A successful semantic run additionally requires compilation against both implementations
and comparisons under the shared trial schedule. Static reachability does not prove that arbitrary
Haskell uses a value on every branch. Handwritten authorship, reference correctness, and meaningful
behavioral coverage still require review; these checks are not a proof or a security sandbox.
