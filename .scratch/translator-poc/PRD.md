# Pure Haskell to monadic cost translation: proof of concept

Status: ready-for-agent

## Problem Statement

Testing lazy evaluation costs with the framework described in *QuickChecking the
Cost of Laziness over Infinite Structures* currently requires handwritten monadic
translations and datatype support. This work is repetitive, and mistakes in
forcing, sharing, or tick placement can invalidate cost properties even when the
translated program produces the right values.

Users need a small translator that preserves the behavior of a supported subset
of pure Haskell and produces inspectable code for an existing monadic interface.
The goal is a proof of concept, not a general Haskell compiler or a profiler of
GHC-optimized execution.

## Solution

A command-line tool accepts one whole Haskell source module, uses GHC to parse,
resolve names, and infer types, and generates a separate readable Haskell module.
The output uses the explicit thunking, forcing, and cost operations exemplified
by the external `hs-pbt` implementation. It includes translated functions,
datatype representations, and structural demand support. Users write their own
QuickCheck properties.

The tool emits a translation log and reports success only after GHC typechecks
the generated module. Unsupported source constructs fail the whole translation
with source-located explanations. The tool does not publish partially translated
code as successful output.

## User Stories

1. As a programmer, I want to select one source module, so that I can translate an algorithm without configuring dependency analysis.
2. As a programmer, I want inferred types to work, so that I need not add signatures solely for the translator.
3. As a programmer, I want ordinary rank-1 polymorphism preserved, so that translated list and tree functions remain reusable.
4. As a programmer, I want readable generated Haskell, so that I can compare it with the paper's translations.
5. As a programmer, I want generated code typechecked, so that successful translation produces a usable module.
6. As a programmer, I want source pattern and guard order preserved, so that translation does not introduce or remove evaluation.
7. As a programmer, I want shared bindings represented by shared thunks, so that reused computations are not charged twice.
8. As a programmer, I want recursive functions supported, so that I can translate ordinary algorithms and infinite producers.
9. As a programmer, I want non-recursive local and top-level values supported, so that ordinary sharing remains expressible.
10. As a programmer, I want local helper functions supported, so that I need not restructure my algorithms.
11. As a programmer, I want local functions to retain surrounding variables, so that their ordinary behavior and sharing survive translation.
12. As a programmer, I want the existing handwritten higher-order implementations reused, so that the proof of concept need not solve general higher-order definition translation first.
13. As a programmer, I want named functions and their compositions accepted as function arguments, so that common mapping and folding examples work.
14. As a programmer, I want direct partial applications at different argument counts supported, so that support is not artificially limited to binary functions.
15. As a programmer, I want simple argument-count discovery without annotations, so that supported code needs no extra source language.
16. As a programmer, I want argument-count checks and adaptations logged, so that the tool's limited analysis is visible.
17. As a programmer, I want replacements selected by resolved identity, so that a locally defined function is not mistaken for a Prelude function.
18. As a programmer, I want a fixed, documented tick policy, so that cost properties have a stable meaning.
19. As a programmer, I want ordinary lazy datatype representations generated, so that I do not write repetitive thunk wrappers myself.
20. As a programmer, I want primitive-valued fields to remain thunked, so that computing their values is not forced prematurely.
21. As a programmer, I want demand types and structural generators and shrinkers generated, so that my datatypes can participate in cost tests.
22. As a programmer, I want finite demand-generation budgets, so that generators do not explore infinite results indefinitely.
23. As a programmer, I want to supply custom demand generators and shrinkers per property, so that I can control test coverage without conflicting instances.
24. As a programmer, I want unused failing computations to remain suspended, so that ordinary lazy error behavior is preserved.
25. As a programmer, I want clear unsupported-feature errors, so that I can distinguish tool limitations from invalid Haskell.
26. As a programmer, I want explicit README limitations and examples, so that I know whether the proof of concept fits my code.
27. As a contributor, I want Stack and a pinned LTS compiler, so that GHC API usage is reproducible.
28. As a contributor, I want the target runtime external to the translator package, so that the tool does not bundle or depend on the reference implementation.

## Implementation Decisions

### Toolchain and translation boundary

- Set up the project with Stack and an explicitly pinned LTS snapshot. Select the
  snapshot after checking GHC API compatibility; no snapshot was selected during
  the interview.
- Use the GHC API for parsing, name resolution, and inferred types. Preserve the
  source evaluation structure; do not let compiler optimizations determine cost
  instrumentation. The exact GHC representation and pass boundary require an
  implementation probe.
- Translate the whole selected module, not a dependency closure selected from
  individual functions. No Cabal/Stack project discovery or user-module traversal.
- Support implicit and explicit Prelude imports only for documented operations.
  Additional imports require a demonstrated need and explicit support; this is
  not blanket approval for containers or other packages.
- Produce a separate generated module and a log. Output naming and CLI spelling
  remain implementation details, not settled interface syntax.
- Keep the target runtime external: do not include `hs-pbt` in the repository or
  declare it as a translator package dependency. Use GHC's existing environment
  to check generated imports. Missing target modules are setup errors; there is
  no custom runtime-path mechanism or automatic dependency installation.

### Semantics and instrumentation

- Translate through an A-normalized form that preserves lazy bindings, source
  pattern/guard order, and sharing. Normalization must not turn lazy bindings into
  eager monadic sequencing.
- Allocate one shared thunk for a shared source value binding within an
  evaluation. Reuse it rather than allocating at each reference. Memorist usage
  sets then avoid duplicate cost accounting.
- Preserve recursive functions and non-recursive local/top-level value bindings.
  The arrangement of top-level shared thunks is an implementation obligation,
  not a new source restriction.
- Charge one entry tick per invocation of a translated source function, including
  local helpers and trivial wrappers. No special optimization analysis of wrappers.
- Retain the instrumentation of supported handwritten library implementations.
  Document exceptions rather than assuming every library operation counts alike.
- Do not tick constructor applications or supported primitives on strict types.
  Force primitive operands as required and retain the costs of computing them.
- Do not tick composition or partial-application adapters. Functions invoked
  through them keep their own instrumentation.
- Preserve ordinary Haskell runtime failure for unmatched patterns and supported
  error cases. Suspend failures inside thunks; do not introduce an error-valued
  monad or promise a cost result for failing computations.
- The handwritten `takeM` corresponds to a separate guard-first source `take`.
  It is not a behavioral reference for a source implementation that matches the
  list before testing the numeric guard.

### Functions and partial application

- Use resolved names, inferred types, explicit parameter counts, supplied argument
  counts, and known library conventions. Require no annotations or arity files.
- Record syntactic parameter-count checks and generated adaptations in the log,
  with function identities and source locations, including on failed translations.
- Support direct partial applications of explicitly defined named functions at
  arbitrary supported argument counts. Do not impose a two-argument limit.
- Support local functions as function arguments, including functions that capture
  surrounding variables. Retain their existing thunks without forcing or ticking
  merely for capture.
- Support composition of supported named functions and validate its interaction
  with partial application. Source anonymous lambdas remain deferred.
- Reuse handwritten source-program higher-order combinators represented in the
  reference library, rather than promising general higher-order definition
  translation. Coverage includes map, foldr, foldl, concatMap, filter, zipWith,
  scanl, composition, and the example's repeatly operation. Validate the final
  inventory and external bindings; repeatly is not automatically a Prelude name,
  and scanl's reference module is marked unfinished.
- Scope this inventory to source-program combinators, not every higher-order
  QuickCheck or monadic implementation helper appearing in the reference code.
- Select handwritten replacements by resolved imported identity, never spelling.
  Translate same-named user definitions normally or report their unsupported
  features.
- The existing binary adapter demonstrates the approach but does not establish
  correctness for all generalized cases. Compile and execute probes before
  implementing the general generator. Reject unsupported function-valued aliases
  and computed function results rather than guessing a calling convention.

### Datatypes and demands

- Generate ordinary lazy algebraic datatype representations, conversion support,
  demand types, and the supporting runtime instances.
- Wrap every ordinary lazy field in a thunk, including primitive-valued fields.
  A primitive value has no internal lazy fields, but computing it can be deferred.
- Generate structural demand generators and shrinkers using the reference
  interfaces, extended with Skip/Need handling. Do not transform arbitrary
  user-written input generators or shrinkers.
- Here “structural” describes how generators/shrinkers are built. The interview
  did not adopt “regular datatype” as a restriction on recursive type parameters.
  Do not silently add that restriction; probe unsupported datatype shapes and
  resolve any necessary additional scope decisions explicitly.
- Generated demands must match the observed constructors. Shrinking must preserve
  compatibility with the result.
- Bound generation by a finite QuickCheck size budget, distribute it among
  demanded fields, and skip fields when it is exhausted. Follow reference
  defaults for weighting where applicable. This bounds generated demands, not
  the source program's time to satisfy them.
- Permit explicit custom demand generators and shrinkers in handwritten tests,
  following normal QuickCheck conventions. Verify the existing target API's
  capabilities before proposing any external-runtime additions.

### Diagnostics and documentation

- Fail whole-module translation for unsupported constructs. Include source
  locations and describe the limitation, not merely a generic compiler failure.
- Distinguish source syntax/type errors, unsupported valid source, unavailable
  target modules, and generated-code type errors.
- Document the supported subset, library inventory, cost policy, log behavior,
  external-runtime setup, and supported/unsupported examples in the root README.
- Preserve the requested follow-up work for multi-module translation and source
  deriving support as local issues. Creating those issues is deferred until the
  requested follow-up documentation step; this specification records the obligation.

## Testing Decisions

The user approved the end-to-end acceptance boundary: translate a source module,
typecheck the output against the external interface, and observe generated code
under explicit demands. Prefer this boundary over tests that mirror AST rewrites.
Use focused lower-level tests only when they isolate a failure the external tests
cannot explain effectively.

- First validate handwritten candidate translations for local captured functions,
  arbitrary-arity partial applications, composition, and shared top-level values.
  Compare types and independently calculated costs. These are feasibility gates,
  not claims of completed validation.
- Check source/generated value agreement on finite inputs and finite observations.
- Compare generated costs to handwritten references only when source demand
  behavior and tick placement match. Sorting references that count comparisons
  are not direct exact-cost oracles for function-entry instrumentation.
- Check repeated uses of the same local and top-level binding charge once.
- Check pattern/guard ordering with unused failures and partially defined values.
  A demanded failure stays a failure; exact GHC exception text need not match.
- Check finite demands on recursive infinite producers, without fully evaluating
  their output during generation, display, or comparison.
- Check generated demand compatibility, finite-budget termination on branching
  structures, valid shrinking, and per-property customization.
- Check missing target modules and unsupported constructs through the CLI/log
  boundary. Failed translation must not appear successful or publish partial
  output as a completed module.
- The external reference has handwritten examples and QuickCheck properties,
  including successful head/tail cost tests restricted to nonempty inputs.
  Those tests do not establish unused-error behavior. Add meaningful regression
  cases rather than assuming this behavior has already been tested.
- Keep external-runtime integration checks explicit. They require the runtime in
  the checking environment without making it a translator package dependency.

## Out of Scope

- Multi-module translation, project dependency discovery, and unsupported imports.
- GHC-optimized execution costs, hardware timing, and configurable instrumentation.
- Generating user cost properties or transforming arbitrary user-written generators.
- Recursive value bindings and knot-tying definitions.
- Anonymous source lambdas and unrestricted higher-order definition translation.
- Function aliases and computed function-returning definitions outside explicitly
  validated translation rules.
- Lazy pattern bindings and explicit irrefutable patterns.
- Strict fields, bang patterns, Strict/StrictData, seq, and strict application.
- Newtype declarations, user-defined type classes, and source deriving clauses.
- Higher-rank types and polymorphic recursion.
- Bundling the target runtime, automatically installing its dependencies, or
  implementing a replacement runtime as part of this translator.
- A formal proof of the translator's correctness.

## Further Notes

### References and decision records

- `paper.pdf`: sections 3, 5, 6.2, and 7 provide translation examples, demand
  interfaces, higher-order adapters, and known runtime limitations.
- Li, Xia, and Weirich, *Reasoning about the garden of forking paths* (2021),
  DOI 10.1145/3473585, is the requested additional interface reference. Its full
  text has not yet been independently reviewed in this session; review it before
  claiming detailed compatibility with that paper.
- The locally supplied `hs-pbt` tree is an external reference inspected during
  planning, not material to add to version control or a validated dependency.
- Architecture decisions are recorded in `docs/adr/0001-source-cost-semantics.md`,
  `0002-ghc-api-and-single-module-boundary.md`,
  `0003-external-monadic-interface.md`, and
  `0004-explicit-thunks-and-limited-function-translation.md`.

### Delivery order

1. Validate the difficult higher-order and sharing cases with compiled probes.
2. Establish the Stack/LTS project and pinned GHC API integration.
3. Deliver a small source-to-generated-code-to-typecheck path.
4. Extend function and datatype coverage with semantic regression tests.
5. Complete demand support, diagnostics, README, and follow-up local issues.

If a feasibility probe contradicts a requirement, bring back the concrete failed
case for a design decision. Do not silently narrow the agreed subset. This
document specifies intended work; no translator, build scaffold, or feasibility
probe has been implemented or verified yet.
