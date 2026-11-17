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

## Translator Source Comments

The translator implementation in `src/` and `app/` contains no ordinary comments
or Haddock. `sh scripts/validate.sh` enforces this with GHC's lexer through
`test/comment_policy.py`; the checker distinguishes comments from string literals
and compiler pragmas, including nested block comments.

This policy does not apply to tests, examples, or generated modules. Strings that
the translator emits as generated comments remain unchanged.

## Execution Pipeline and Sandbox Isolation

When `cost-translate-test` starts, `test/Main.hs` discovers all semantic fixtures (`discoverReferenceCases`) into a typed `Group` registry with no arbitrary `IO` callbacks and runs `validateReferenceCases` across the entire repository **before** executing any selected group. Even a single-case run fails immediately if any fixture directory, `Case.hs`, `Reference.hs`, or `.cabal` test-suite stanza violates policy.

For each semantic fixture, `ReferenceRunner` creates two isolated sandboxes under a temporary run directory (`generated/` and `reference/`):

1. Both sandboxes receive the pure source fixture (`Example.hs`, or a registered alias from `sourceAliases` in `test/ReferencePolicy.hs`) and the shared `Case.hs`.
2. In `generated/`, `Cost.Translate.translateFile` translates the source module into `Generated.hs`. Before compiling any callers, `GeneratedFunctions` parses the source and published output with their language pragmas and requires every parameterized top-level source function to have a module-level generated binding under its translated name. Signatures, record selectors, and nested bindings do not satisfy this check. `GeneratedConstruction` then rejects references to computation runners and result-reasoning operations. In `reference/`, `ReferenceRunner` copies the handwritten `Reference.hs` to `Generated.hs`; handwritten references are not subject to these generated-output checks.
3. `ReferenceRunner` compiles `test/reference/Observe.hs` against `Case.hs` and `Generated.hs` in each sandbox using `ghc -dynamic -Werror=unused-imports -package MemoristANT -package QuickCheck`.
4. The shared controller (`test/reference/Compare.hs`) starts both `observe` binaries as line-buffered child processes over `stdin`/`stdout` (`test/reference/Protocol.hs`), compares them across fixed boundary `Sample` inputs and 200 seeded QuickCheck trials with shrinking, and writes `comparison.log`.
5. After comparison passes, `test/GeneratedChecks.hs` runs any applicable post-comparison artifact checks on `generated/` (`-Wall -Werror` and HLint on `generated-warnings*`, code-size bounds on `pattern-scaling`, maximal-application arity logs on `foldr-arity-four-partial`, single-inspection constructor grouping on `ordered-equations-and-conjunctive-guards`, and terminal ordinary-`do` flattening on `anf-demand` and `manual-terminal-do`). The terminal-`do` checks also look through parentheses and unannotated `let … in` wrappers, but stop at type annotations and control-flow boundaries. The paired binder pre-check on `anf-scoped-renaming` runs before comparison.

Compiling the two workers in separate sandboxes ensures that both sides can declare `module Generated` and define representation types (`*T`), demand types (`*D`), and typeclass instances (`Repr`, `Demand`, `FoldableT`) without symbol or orphan-instance collisions, while still executing the exact same `Case.hs`. Dynamic linking (`-dynamic`) keeps temporary executable disk usage modest and leaves the root library and `cost-translate-test` executable free of `MemoristANT` and `QuickCheck` Cabal build dependencies.

The location check covers all source variants automatically, including functions
that are unused or absent from explicit source exports. It distinguishes
parameterized functions from zero-argument value bindings and accepts multiple
equations for one function. Failures report the source function and location,
expected translated identifier, and generated artifact. This structural check
does not prove ordinary calling types or the absence of hidden setup: direct
caller compilation and behavioral comparisons check separate contracts. The
`top-level-function-interface` fixture uses the same ordinary caller and
handwritten reference with and without an unused top-level value.
The `top-level-independent-functions` fixture also checks transitive calls among
such functions, suspended fields, repeated demands, and fresh invocations. Its
relative-name and caller-allocation observations reject hidden initialization
inside an otherwise ordinary-looking function.
The `top-level-value-actions` fixture also calls functions that use a global
literal and calls the global action itself through `MemoristM Int`. The literal
has no producer entry tick, so its direct, uncached, and explicitly cached probes
can compare exact source costs without relying on global work sharing. An unused
bottom global remains safe; demanding it must fail.

Global work sharing is a known Memorist limitation tracked in
[issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2).
The `top-level-function-sharing` reference retains the source costs 4/7/4 and
cross-export cost 3. Ordinary generated actions currently produce 5/10/5 and 4
respectively. These are genuine comparison failures, not passing source-cost
assertions. Comparisons remain strict, so selectors affected by this accepted
runtime limitation still fail; their costs have not been changed or exempted.

Handwritten references expose the same direct caller interface while retaining
private shared contexts for the ideal source-global model. Those contexts use
reference-only reasoning operations, enclosing all global allocations beneath a
negative root so they cannot collide with ordinary caller allocations. Generated
bindings must not use this reference-model technique.

The `generated-functions` selector checks small positive and negative parser
specimens and the shared lifecycle. Its lifecycle test runs the ordinary
`identity` fixture in a child process with a private GHC wrapper. The wrapper
runs the real compiler, then replaces the verified candidate with a specimen
missing the expected function. The test requires the location diagnostic before
caller compilation starts. This is harness sensitivity evidence, not a semantic
reference comparison; it requires the external runtime environment.

Generated code constructs Memorist computations. It must not run them or extract
their annotated results. `GeneratedConstruction` checks resolved GHC names in
every published generated artifact before caller compilation. It rejects
`eval`, `eval1`, the `Annotated` projections (`value`, `annot`, `costAnnot`,
`usageAnnot`), and the raw computation runners (`runM`, `runMMBT`, `runMBT`,
`runAnnotT`, `runMT`). It also rejects the definitions exported by
`Memorist.Infer.Infer`, including `infcost`, `calcCost`, and `correct`: these APIs
run or inspect computations rather than construct them. The rule includes uses
inside generated helpers that are never called. Renaming an import, importing
the operation without qualification, or assigning it to a local alias does not
exempt it. Record updates of forbidden fields and patterns that deconstruct the
runtime `Annotated` constructor are also rejected. The check resolves ambiguous
update fields through GHC's typechecked syntax rather than banning their spelling.
Unrelated user fields or constructors and `Annotated` type mentions remain legal.
An unrelated local function named `value` is not a violation.

This boundary applies to generated support code as well as translated source
bindings. The shared observer and independent reasoning checks may use these
operations to inspect results and measure demanded costs. The check does not
ban thunk operations such as `lazy`, `force`, or `tval` used by representation
conversions. Contract specimens and publication-mutation tests check that the
guard rejects reasoning operations even when a generated caller would otherwise
compile and return the correct value and cost.

Run this contract and its lifecycle sensitivity test with the external runtime
available:

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments="generated-construction"
```

The guard checks references to these runtime definitions. It is not a proof
against arbitrary reimplementations of an interpreter.

The `polymorphic-local-value` and `polymorphic-local-demand` fixtures exercise
native allocation for unconstrained local values with type-variable or nested-list
results. Their independent references retain the reasoning operations permitted
on the reference side. The generated implementations allocate once and convert
representations without changing native thunk names or annotations. The probes
check costs across type instantiations, fresh invocations, suspended bottom
elements, partial spine demands, and relative root/field identities.
Both explicit local quantifiers and inferred polymorphic locals must preserve
annotations that refer to outer scoped type variables.

`polymorphic-local-demand/Example.hs` deliberately shadows an outer `forall b`
with a local `forall b`. Its `-Wno-name-shadowing` pragma preserves that scope
regression; the handwritten reference and caller need no warning exception.
This does not expand the bounded generated-output warning checks.

The `constrained-sharing-local` fixture checks native allocation for a
dictionary-sensitive local used at closed scalar types. Its original handwritten
reference allocates separate `Int` and `Integer` thunks inside each call. The
generated implementation uses GHC's instantiated expression types to select and
reuse the corresponding native thunks; it does not run computations or construct
runtime names. Missing or ambiguous type evidence prevents native specialization.

The `constrained-local-demand` fixture repeats each dictionary specialization,
distinguishes separate source bindings, and checks freshness between invocations.
Its generated callers also cover explicit and inferred quantifier scopes, a
generic caller without added reflection constraints, partial demands, unused
bottoms, and demanded failures. Relative identity probes detect per-reference
allocation and accidental reuse across bindings without fixing runtime names.

The `constrained-local-abstract` fixture uses one local polymorphic value twice
at the enclosing function's abstract type. Its generic caller supplies only the
existing `Eq`, `Num`, and `Scalar` constraints, not `Typeable`. The independent
native reference checks shared field identities, invocation freshness, unused
bottom inputs, and partial demands. At both `Int` and `Integer`, full demand
costs three ticks: one function entry and two producer entries. Skipping both
fields costs one. Eligibility follows checked lexical type-lambda binders, not
their printed names. A helper's fresh quantified variable cannot determine an
allocation declared outside that helper.

The `constrained-helper-abstract` fixture declares the shared value inside a
signed local helper and uses it only at that helper's abstract type. Generic
callers still require no `Typeable`. One helper call costs four ticks including
the outer entry; two helper calls within one outer invocation cost seven, not
five. The nested result probes require aliases within each call and disjoint
identities between helper calls. They also check partial demand: skipping both
helper results costs one tick, while demanding their roots without scalar fields
costs three. The original single-helper probes retain their separate expectations.

The `constrained-helper-inferred` fixture exercises an unsigned helper whose
fresh type variable comes from GHC's inferred binding group. Its generic caller
requires no `Typeable`. Both arithmetic result fields reuse one producer, so full
demand costs eight ticks and skipping both fields costs two. The field thunks
themselves are distinct; demanded costs observe sharing of the producer beneath
them. Lexical ownership includes only group binders occurring in that helper's
checked monotype, not every variable generalized for its sibling bindings.
Two calls to the inferred helper within one outer invocation cost fifteen ticks;
skipping both calls costs one, and demanding their roots without arithmetic
fields costs three. These probes detect a producer incorrectly shared between
helper invocations even when the output field thunks remain distinct.

The `constrained-local-maybe` fixture uses a constrained `Maybe a` local twice
at `Maybe Int`. Its unconstrained generic caller also checks that the unused
input type needs no reflection constraint. The independent native reference
shares both the `Maybe` root and its payload: full demand of `Just` costs four
ticks, root-only demand costs two, and skipping both roots costs one. `Nothing`
costs two. Relative identity probes check root and payload reuse without fixing
runtime names, plus fresh identities across calls that reuse caller inputs.
Fresh calls pay four ticks each; repeated demands on an explicitly cached call
pay four in total. Skipped bottom counts and suspended payload failures remain
safe, while demanding either failure must raise.

The `constrained-maybe-abstract` fixture uses the same `Maybe` local twice at one
scoped abstract element type. Its generic caller requires only the existing
`Eq`, `Num`, and `Scalar` constraints, not `Typeable`. Both `Int` and `Integer`
callers compare against an independent native reference. Full `Just` demand
costs four ticks, root-only demand costs two, and skipping both roots costs one.
The generated allocation keeps the complete `Maybe a` type; eligibility checks
the element's checked lexical binder rather than its printed name. Repeated fresh
calls pay four ticks each, while repeated demands on a cached call pay four in
total. Relative root/payload identities remain shared within each invocation and
disjoint between invocations. Bottom counts and suspended payload failures remain
safe when skipped and raise when demanded.

## Private Runtime Allocator

Apply the [external runtime patch](../patches/README.md) and rebuild
`MemoristANT:lib` before running runtime-dependent checks. The `runtime-allocator`
selector compiles clients against that real package with `base`, `MemoristANT`,
and `mtl` explicitly exposed. The root test executable still has no runtime or
`mtl` build dependency.

The negative clients must fail for the expected missing capability or private
name. They cover `get`, `put`, `gets`, `modify`, `modify'`, and `state`; import and
type aliases; generic and transformer wrappers; the former `Memorist`
superclass route; and the exact save/reset/allocate/restore helper. Both public
runtime implementations also reject direct constructor wrapping, record updates,
safe coercion, and access to allocator helpers. A hidden package, syntax error,
or unrelated compilation failure does not satisfy the expected diagnostics.
The runner also rejects unregistered client specimens rather than silently
skipping them.

At O0/O2, positive clients execute ordinary lazy/strict State operations, native
allocation in both runtime implementations, and varied scalar inputs with known
values and demanded costs. Two distinct one-tick computations cost two; repeated
forces of one retained thunk cost one. Unused bottoms stay suspended, and two
allocations have distinct names. Observers may inspect results with the existing
reasoning operations; generated code remains subject to the construction guard.
A handwritten runtime model also retains the required direct/uncached/cached
costs 4/7/4; it is a runtime preservation check, not a generated-interface test.

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments="runtime-allocator" --ghc-options='-Wall -Werror'
```

The restriction protects access to a live allocator through ordinary safe
construction. It does not assert thunk authenticity or forbid reasoning about
separate evaluations. It adds no generated state-scoping helper or global naming
policy, and does not by itself solve direct top-level sharing translation.

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
