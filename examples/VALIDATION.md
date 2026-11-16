# Review and Manual-Example Validation (`f42170e..4828860`)

## Pinned Review Range and Scope

- **Base / merge-base commit**: `f42170ecc5455cb5926f892d12f8e2024a9e7f76`
- **Reviewed head commit**: `4828860473da4f9d2ce457da29215972784c3cdf`
- **Artifact manifest SHA-256**: `d9c8dec8a0d405bdf39daaf6c2ea964ef4332636ad62c8eb47c9417e408c5bf3`
- **Scope note**: The compiler-selection commit `c072058` landed during this work and is preserved. The review excludes it and stays pinned to the range above: 15 commits containing 5 production translator fixes and 10 test-infrastructure, coverage, policy, CI, and documentation commits.

The independent `Standards` and `Spec` reviews were integrity-validated against the artifact manifest above. Each covered **31 of 437 changed paths plus one unchanged rule/specification document**:

- **3 production translator modules**: `src/Cost/Translate/Callable.hs`, `src/Cost/Translate/Frontend.hs`, `src/Cost/Translate/Output.hs`.
- **10 shared test and reference-harness modules**: `test/Main.hs`, `test/FixturePolicy.hs`, `test/GeneratedChecks.hs`, `test/ReferencePolicy.hs`, `test/ReferenceRunner.hs`, and the 5 modules in `test/reference/` (`Compare.hs`, `DemandSupport.hs`, `Observe.hs`, `Protocol.hs`, `ReferenceSupport.hs`).
- **9 build, CI, and rule/specification documents**: the workflow, two Cabal manifests, `README.md`, `docs/testing.md`, three thunk-reuse/convention documents, and `AGENTS.md` (Standards) or `PRD.md` (Spec).
- **10 representative source fixtures**: ANF demand and scoped-renaming inputs, ordered equations, scoped case/guard parameter and factory inputs, argument forwarding, and case/guard matching. Individual fixture references and probe catalogs were outside this read-review scope.

The remaining **406 changed paths** (mostly mechanical fixture migrations and policy/reference specimens) were not individually reviewed; this document does not claim a full-range review pass across all 437 files.

## Standards

Accepted integrity-validated independent review over the bounded 32-path subset. No hard violations were found. Two **LOW** judgment calls were noted and intentionally left unchanged:

1. **Redundant full inventory discovery (`test/Main.hs:47` and `test/Main.hs:58`)**: `main` calls `discoverReferenceCases` at line 47 to construct the selector registry and then calls `validateReferenceCases` at line 58, which immediately re-runs `discoverReferenceCases`.
2. **Duplicate infrastructure fixture catalog (`test/ReferencePolicy.hs:64` vs. `test/FixturePolicy.hs:33`)**: `infrastructureFixtures` in `test/ReferencePolicy.hs` enumerates the same `fixture-policy-*` specimen directories that `checkFixturePolicy` lists in `test/FixturePolicy.hs`.

## Spec

Accepted integrity-validated independent review over the bounded 32-path subset. No deviations from the specification or approved conventions were identified in the reviewed required paths.

## Five Manual Fix Examples and Red/Green Verification

Each of the 5 production fixes in `f42170e..4828860` is paired with a standalone module in `examples/` and a semantic reference case in `test/cases/manual-*/` (`Case.hs` and `Reference.hs`, 15 files total).

Pre-fix compilers were built in isolated sandboxes from git archives of each fix's parent commit (`b5f21ad^`, `446f2c1^`, `f615f65^`, `dcf55d1^`, `d892e2f^`) using GHC 9.10.3 with `-Wall -Werror`. Each pre-fix compiler was run against the exact current `examples/*.hs`, `Case.hs`, and `Reference.hs` files (all 15 file SHA-256 digests verified identical) through the shared `Compare`/`Observe` harness and `GeneratedChecks`:

| Fix | Pre-fix commit | Source | Selector | Pre-fix (red) result | Current (green) result |
| --- | --- | --- | --- | --- | --- |
| `b5f21ad` | `b5f21ad^` | `examples/ThunkArguments.hs` | `manual-thunk-arguments` | Fails reference comparison on caller-level allocation and thunk-identity (`tname`) snapshots | PASS (boundary + 200 seeded trials) |
| `446f2c1` | `446f2c1^` | `examples/ThunkMatching.hs` | `manual-thunk-matching` | Fails reference comparison on caller-level allocation and thunk-identity (`tname`) snapshots | PASS (boundary + 200 seeded trials) |
| `f615f65` | `f615f65^` | `examples/ScopedAnnotations.hs` | `manual-scoped-annotations` | Fails translation at candidate typechecking (`M.Th` annotation captured by inner `forall a`) | PASS (boundary + 200 seeded trials) |
| `dcf55d1` | `dcf55d1^` | `examples/ConstructorEquations.hs` | `manual-constructor-equations` | Passes value/cost comparison, then fails `checkOuterListInspection` (`chooseM` has 2 outer forces and 0 combined `NilT`/`ConsT` cases) | PASS (boundary + 200 seeded trials + AST check) |
| `d892e2f` | `d892e2f^` | `examples/TerminalDo.hs` | `manual-terminal-do` | Passes value/cost comparison, then fails `checkTerminalDoBlocks` (`chooseM` has 1 directly nested terminal ordinary `do`) | PASS (boundary + 200 seeded trials + AST check) |

`tname` checks logical thunk identity; fresh-name observations cover the current
allocator level, not GHC heap allocation. Ticks count source-function entries
under the stated demands, not elapsed execution time. These bounded tests do
not constitute a correctness proof.

## Warning, Lint, and Test Suite Status

- **Manual example and fixture compilation/lint**: All 5 new `examples/*.hs` modules and all 10 new `test/cases/manual-*/{Case.hs,Reference.hs}` modules compile cleanly under `ghc -Wall -Werror` and pass `hlint` with no hints.
- **Shared reference-support warning enforcement**: `head boundaries` in `test/reference/Compare.hs` (which triggered `-Wx-partial` under `-Wall -Werror`) was replaced with an explicit `case` match, and all 5 modules in `test/reference/` now carry `{-# OPTIONS_GHC -Wall -Werror #-}`. This enforces warning-free compilation of the shared observer/comparator support modules without expanding generated-output `-Wall -Werror` checks beyond `generated-warnings` and `generated-warnings-minimal`.
- **Repository HLint check**: `hlint` reported no hints across `src`, `app`, `test/*.hs`, `test/reference/*.hs`, the 5 new `examples/*.hs` modules, the 10 new `test/cases/manual-*/{Case.hs,Reference.hs}` files, `integration/probes`, and `integration/test-integration`.
- **Package check**: `cabal check` exits `0` with existing distribution advisories (missing metadata fields and dependency upper bounds, outside compiler/lint warning scope). `stack sdist .` includes all five manual sources and excludes generated examples, logs, `.hi`, and `.o` files; `fixture-policy` and all five manual selectors pass against extracted package data outside the checkout. The `.` scopes packaging to the translator; unqualified `stack sdist` also checks the external runtime and fails on its pre-existing package metadata omissions.
- **Accepted warning exceptions**: existing static fixtures deliberately retain partial `head`/`tail` calls and redundant guards (`-Wx-partial` and `-Woverlapping-patterns`). The new scoped example suppresses intentional type-variable shadowing, as documented inline and in the README. The macOS linker's redundant `-U` warning remains a toolchain exception. No maintained runner/support warning was accepted.
- **Integration suite (`stack --stack-yaml integration/stack.yaml test cost-translate-integration`)**: PASS across all 4 suites (`cost-translate-integration` 37 P1–P7 probes, `runtime-scanl` 9 checks, `runtime-higher-kinded`, and `runtime-polymorphic-sharing`).
- **Runtime-free validation**: `scripts/validate.sh` passes with GHC 9.10.3 and fatal warnings. All 16 compiler-preflight self-tests also pass; these checks preserve the separately committed compiler-selection work rather than extending the pinned source review.
- **Manual README replay**: all 26 documented GHCi observations pass, including the local unary wildcard/variable guard barriers.
- **Full root suite (`stack test cost-translate:test:cost-translate-test --ghc-options='-Wall -Werror'`)**: PASS, including the strengthened local unary guard-barrier probes, all five manual cases, diagnostics, selector/policy gates, and harness fault-injection self-tests.
