# Source policy and behavior-preserving refactoring

Status: ready-for-agent

This specification records the decisions from the design session. Implement the
checker, integrate its enforcement, and refactor the implementation to comply.
The checker and integrations have not been implemented by the design session.

## Goal and scope

Enforce the six limits below deterministically, and make every file and directory
under `src/` comply without changing translator behavior.

The six limits apply **only to `src/`, recursively**. Everything outside `src/`,
including `test/`, is exempt from these six limits. This exemption does not remove
existing lint, warning, or test requirements. ShellCheck has its own broader scope.
Files placed under `src/` remain subject to the limits even if they are generated.

The user will pause the other agent before invoking this specification. Start
from the resulting checkout and preserve its changes, especially the work for
<https://github.com/lastland/hs-monadic-cost-translator/issues/1>.

## Execution plan

### 1. Establish the starting state and toolchain

- Read the current repository instructions, implementation, and tests. Code is
  authoritative; this specification does not assert that the checkout is unchanged.
- Inspect Git status and identify existing changes before editing or staging.
- Record the starting commit and distinguish existing test failures from new ones.
- Use GHC 9.10.3, matching the package's compiler requirement.
- Use HLint 3.10 and ShellCheck 0.11.0. Select and pin a Fourmolu version after
  verifying that it parses and formats the current implementation correctly.
- Ask the user to install missing tools. Missing or incompatible tools must not
  silently disable checks.
- Verify a committed Fourmolu configuration, using an 80-column target and the
  existing four-space indentation style. Check formatting stability before
  adopting the version/configuration combination.

Completion: the starting state and baseline test results are recorded, and the
required tools and formatter configuration have been verified.

### 2. Format before measuring

**Always run Fourmolu before checking any per-file limits.** Reformat a file after
each refactoring change and before measuring it again. This order applies to
initial measurements as well as subsequent compliance checks.

Local formatting may modify explicitly selected `src/` files. Read-only checks,
Git hooks, and CI must require Fourmolu-clean input before running per-file
metrics; they must not rewrite files. A formatting failure must prevent metrics
from running on that unformatted input.

Fourmolu's column limit is best-effort. Keep the separate, strict 80-character
check. If the selected formatter configuration cannot produce stable formatting,
show the problem and resolve it with the user rather than weakening enforcement.

Completion: initial file measurements describe Fourmolu-formatted source, not the
old layout. Discard previous unformatted size estimates as compliance evidence.

### 3. Implement and test the checker

Use the definitions and regression requirements below. Reuse existing tools and
repository patterns. In particular, use GHC's parser and lexer rather than a
regular-expression approximation of Haskell syntax.

Add regression tests before relying on the checker. Demonstrate that each rule
rejects a violating specimen and accepts its compliant counterpart.

Completion: all six rules, discovery, formatting order, failure handling, and
deterministic reporting have passing regression tests.

### 4. Refactor by responsibility

Use the formatted checker report to guide changes. Likely areas include:

- `Cost.Translate.Output`: partition structural builders by syntax responsibility
  while retaining a small caller-facing interface through direct re-exports.
- `Cost.Translate.Frontend`: separate source preparation, validation, name
  planning, module assembly, and expression/local-binding translation.
- `Cost.Translate.Datatypes`: separate type rendering, conversions, demand
  operations, selectors, and common support where the measured limits require it.
- `Cost.Translate.Specialize`: extract its large nested analysis and selection
  operations without changing their discovery or substitution order.
- Other modules: resolve oversized definitions, repeated literals, and long lines
  wherever the checker finds them.

Keep mutually recursive operations together where practical. Before proposing a
new interface to break an import cycle, verify that it parses, typechecks, and
supports its intended callers. Defer substantive design choices to the user.

Consolidate genuinely shared literals or AST builders within the responsibility
that owns them. Preserve the distinction between unrelated generated names. Use
meaningful modules and helpers rather than numbered fragments, forwarding
wrappers, or a global collection of unrelated string constants.

Keep implementation code under `src/`; moving it elsewhere is not compliance.
Check directory budgets as modules are added. Register internal modules in
`cost-translate.cabal`, and keep the public translator interface stable.

Completion: every reformatted source file passes all applicable limits, the
module structure remains coherent, and affected behavioral tests pass.

### 5. Integrate enforcement and verify the result

Implement the script, hook, CI, and agent-workflow responsibilities below. Run the
complete acceptance checks. Commit only changes belonging to this task, in
reviewable increments. Keep unrelated changes intact.

Completion: all acceptance criteria pass, or a specific baseline/environment
blocker is reported without claiming completion.

## Exact counting rules

All limits are inclusive: the limit itself passes; the next value fails.

| Rule | Limit | Definition |
| --- | --- | --- |
| File size | 500 LOC | Count physical lines occupied by code tokens. Exclude blank and ordinary comment-only lines. Include module headers, imports, signatures, and compiler pragmas. |
| Top-level definitions | 20 | Count module-level value definitions, including zero-argument constants and pattern bindings. Multiple equations of one binding count once. A pattern binding counts once, not once per introduced name. |
| Definition size | 50 LOC | Check named value definitions, local definitions, class/instance method implementations, and lambdas. Count all equations and nested code within the definition. Do not add its separate type signature to its span. |
| Line width | 80 characters | Check every physical line, including comments, pragmas, and literals. Count Unicode code points, expand tabs at eight-column stops, include trailing whitespace, and exclude the line-ending delimiter. |
| Repeated string literal | 2 occurrences/file | Group actual Haskell string literals by decoded contents, not source spelling. Include empty and one-character strings. There is no minimum length or occurrence-context exemption. |
| Directory size | 20 immediate entries | Count files plus directories immediately inside each directory under `src/`, including `src/` itself and hidden entries. Do not count all descendants against their ancestors. |

Additional interpretation:

- Signatures, imports, re-exports, and compiler-generated record selectors are
  not value definitions for the top-level count. Explicit methods are subject to
  the size limit but are not direct module-level value declarations.
- An outer definition includes its local definitions and local signatures.
  Each local definition also receives its own size check.
- Multiline literal contents count as code, even on visually empty interior
  lines. Multiple tokens or definitions on one physical line do not multiply
  that line's LOC contribution.
- Count string literals in expressions, patterns, type syntax, annotations, and
  Haskell quotation syntax. Quotes inside ordinary comments or opaque quasiquote
  text are not independently parsed Haskell string literals. Compiler-option text
  is not automatically a Haskell string literal merely because it contains quotes.
- For example, `"a"`, `"\97"`, and `"a"` are three occurrences of the same decoded
  literal and must fail. Multiple equations such as `f 0 = 0` and `f n = n`
  constitute one top-level definition.

There are no approved per-file exceptions, inline suppressions, or grandfathered
violations for the six limits.

## Checker implementation requirements

- Reuse the standalone GHC-checker pattern in `test/CommentPolicy.hs` and
  `test/comment_policy.py`, and the parser approach in
  `test/GeneratedFunctions.hs`. These are references, not necessarily interfaces
  that should be imported unchanged.
- Explicitly establish GHC2021 before applying each file's pragmas. The existing
  standalone parser helpers do not themselves establish the Cabal language.
- Use the parsed AST to identify declarations, grouped equations, local bindings,
  methods, and lambdas. Use lexer tokens to classify code-bearing lines and
  decoded string literals. Count each lexical occurrence once.
- Preserve physical source positions. Logical locations changed by source
  directives must not hide code or falsify counts.
- Discover the complete `src/` tree directly, including untracked files, without
  depending on Git's tracked-file list. Support source distributions without Git.
- Sort traversal and diagnostics. Report repository-relative paths, physical
  locations where applicable, the failed rule, the actual value, and the limit.
  Do not include nondeterministic temporary paths or timing data in diagnostics.
- Fail explicitly on unreadable files, invalid encoding, lexer/parser failures,
  missing source roots, unsupported source inputs, and required-tool failures.
  Never treat an unexamined input as compliant.
- Handle LF, CRLF, and a missing final newline consistently.
- Define and test a safe symlink policy; rejecting symlinks is preferable to
  silently skipping them or following paths outside the checked tree.
- Do not reuse the comment checker's CPP-directive masking as structural
  analysis. CPP, preprocessing, unfamiliar source kinds, and primitive byte-string
  comparison require an explicit policy before support is claimed. Ask the user
  if encountered; unsupported inputs must fail clearly in the meantime.
- Keep thresholds in one maintained implementation/configuration source. Agent
  instructions and hook wrappers must not contain independent checker logic.

Suggested implementation locations are `test/SourcePolicy.hs`,
`test/source_policy.py`, and specimens under `test/source-policy/`. These are
planned deliverables, not existing tools. Follow existing source-distribution
registration and temporary-build conventions; compile maintained checker code
with `-Wall -Werror`.

## Tools and enforcement locations

Keep existing frameworks where they fit:

- **Fourmolu:** mandatory formatting and read-only formatting verification.
- **HLint 3.10:** existing Haskell lint coverage, including suggestions.
- **ShellCheck 0.11.0:** maintained `.sh` files across the repository, including
  the executable shell fixture `test/generated-functions/PublishMissing.sh`.
  Include newly added scripts automatically. Exclude external dependencies and
  generated/build artifacts through a documented discovery policy.
- **GHC 9.10.3:** syntax-aware metrics and warning-clean compilation.

Pin tool versions and commit their configuration. Ensure that user-global
configuration cannot silently change results. Missing tools or wrong versions
are errors. Compiler/linter exceptions and suppressions require explicit user
agreement and a recorded explanation.

The following script interfaces describe the intended responsibilities. Keep
wrappers small and reuse one implementation of each check.

| Entry point | Responsibility |
| --- | --- |
| `scripts/format-source.sh [FILE...]` (new) | Modify selected `src/` Haskell files with pinned Fourmolu; default to all scoped source files. |
| `scripts/check-source.sh` (new) | Read-only Fourmolu verification first, then all six checks across `src/`. |
| `scripts/check-shell.sh` (new) | Read-only ShellCheck over maintained shell files, using each script's declared shell. |
| HLint wrapper or existing invocation | Preserve the current `src`, `app`, `test/*.hs`, and `test/reference/*.hs` coverage. Local and CI runs must use the same version and configuration. |
| `scripts/validate.sh` (existing) | Aggregate source/shell checks, checker self-tests, existing source policies, the warning-clean build, and runtime-free tests. Arrange HLint execution consistently with this validation and CI. |
| `scripts/stack-test.sh` (existing) | Remain the required entry point for Stack tests, including the full semantic/reference suite. |

Scripts must propagate failures, handle paths safely, and pass ShellCheck
themselves. Read-only checks must not autoformat, restage, or edit source files.

### Git hook

Provide one thin, **opt-in** `pre-commit` hook that invokes the shared source,
shell, and HLint checks. Check the staged snapshot, including complete source
files, directory structure, scripts, and configuration. Do not inspect only
changed lines or substitute unstaged fixes for staged contents.

The hook must preserve the index and working files. It must not autoformat,
restage, stash, or run the expensive semantic suite. Installation or replacement
of hooks and changes to Git configuration require explicit approval. A hook
manager is unnecessary unless it provides a verified benefit. No `pre-push` hook
is required by this specification.

### CI

**CI enforcement is mandatory, not just local enforcement.** For every push and
pull request, CI must run the shared read-only checks against the full checkout:

1. Verify Fourmolu formatting before measuring source files.
2. Enforce all six limits across the complete `src/` tree, including directory
   sizes. Any violation must fail CI; a report-only job is insufficient.
3. Run ShellCheck over maintained shell files and retain the existing HLint
   coverage.
4. Run the checker, discovery, formatting-order, script, and hook regression
   tests, plus the existing warning-clean build/runtime-free validation.

Provision the pinned tools explicitly. Missing tools, incompatible versions, or
checker failures must fail CI. Do not restrict enforcement to changed lines or
let path filters skip the full-tree checks. Check current official CI
documentation before editing workflow configuration.

Skipping a local hook must not suppress the CI checks. Changing GitHub repository
settings or required-check rules is not authorized by this specification.

This specification covered runtime-free CI only. The [current CI workflow](../../.github/workflows/ci.yml)
also runs the handwritten-reference and integration suites against the patched
`hs-pbt/` submodule.

### Agent workflow

Agent instructions should invoke the scripts rather than duplicate the limits:

1. Edit a source file, run Fourmolu, then check metrics and lint results.
2. After shell changes, run the shared ShellCheck command.
3. After each coherent extraction, compile and run affected behavioral tests.
4. Before completing substantial work, run full validation and reference tests.
5. Commit only changes belonging to the task.

Keep judgments about coherent module interfaces, independent reference behavior,
and accepted exceptions in the workflow. Every repeatable requirement should
have a deterministic tool check rather than rely solely on agent instructions.

## Required regression coverage

| Area | Required evidence |
| --- | --- |
| Exact thresholds | Accept/reject pairs for 500/501 LOC, 20/21 top-level definitions, 50/51 definition LOC, 80/81 characters, 2/3 literal occurrences, and 20/21 immediate entries. |
| Haskell structure | Constants, operators, multiple equations, guarded bodies, pattern bindings, local `let`/`where` definitions, nested scopes with the same name, methods, lambdas, and separate signatures. |
| Lexical cases | Blank and nested-comment lines, comment markers inside strings, multiline literals/string gaps, empty/short strings, escaped-equivalent strings, Unicode, tabs, trailing whitespace, CRLF, and no final newline. |
| Scope and discovery | New/untracked nested source files; violations outside `src/` ignored by the six-rule checker; mixed file/directory children; source distributions without Git; newly added maintained shell scripts. |
| Failure handling | Bad syntax/encoding, unsupported inputs, unreadable paths, missing/wrong tool versions, missing source roots, symlinks, and source-location/preprocessor cases that could hide violations. |
| Formatting order | Unformatted input fails before per-file metrics run; formatted input reaches the same metric engine used for real source. |
| Determinism | Repeated runs and different enumeration orders produce identical ordered results. |
| Hook lifecycle | Bad staged/good unstaged contents fail; good staged/bad unstaged contents do not contaminate the staged result; staged shell failures and directory overflows fail; checks leave source files and index entries unchanged. |

Use independently specified expected counts and generated varied specimens where
practical. Do not merely test that a command exits nonzero; assert the intended
rule, location, actual value, and threshold. Exercise real script/hook paths as
well as individual helper functions.

## Behavioral preservation and acceptance

Preserve the translator's public interface, supported inputs, generated module
structure, diagnostics, verification/publication behavior, laziness, sharing,
polymorphism, dictionary behavior, and tick policy.

Use the independently handwritten references already exercised through
`test/ReferenceRunner.hs` and `test/reference/Compare.hs`. Preserve fixed-seed,
varied-input comparisons. Before/after generated-output comparisons are useful
additional evidence, not a replacement for the independent references.

Retain issue #1's direct module-level function and sharing regressions. Its
contract includes a result of 20 at cost 4 for the documented shared-value
example, cost 7 for two uncached calls in one Memorist computation, and cost 4
per separate evaluation, measured with `infcost`/`calcCost`. If the paused
starting state fails these tests, identify the baseline failure and consult the
user instead of silently taking over that issue or claiming a regression fix.

Completion requires all of the following:

- Every scoped source file is Fourmolu-clean and all six limits pass.
- Checker, formatting-order, discovery, script, and hook regressions pass.
- HLint and ShellCheck report no unapproved diagnostics.
- Maintained source and checker code compile without warnings under
  `-Wall -Werror`. Keep generated-output warning policy unchanged unless the user
  separately agrees to change it.
- `sh scripts/validate.sh` passes.
- `sh scripts/stack-test.sh` passes, including the full reference suite and the
  retained issue #1 regressions.
- CI is configured to fail on formatting violations, any of the six source
  limits, ShellCheck/HLint diagnostics, or checker regression failures, and its
  required validation jobs pass. Distinguish configuration/local verification
  from an actual CI run; do not claim a remote run without observing it.
- The checker and required sources/specimens are included in source
  distributions, and new internal modules are registered with Cabal.
- The final report separates verified results, baseline failures, assumptions,
  and remaining blockers. Do not claim tests ran when they did not.

For any behavioral fix authorized during this work, explain the affected
behavior and root cause, show why the fix addresses the general problem, and add
a regression that fails without the fix and passes with it. Keep routine updates
short and concrete; ask before changing agreed design or terminology.

## Implementation references

- `cost-translate.cabal`: GHC version, GHC2021, module inventory, and packaging.
- `test/CommentPolicy.hs`, `test/comment_policy.py`: standalone GHC lexer checker
  and temporary compiler/self-test driver.
- `test/GeneratedFunctions.hs`: parsing file pragmas and module-level bindings.
- `test/ReferenceRunner.hs`, `test/reference/Compare.hs`: successful-translation
  lifecycle and independent, reproducible behavioral comparisons.
- `scripts/validate.sh`, `scripts/stack-test.sh`, `.github/workflows/ci.yml`:
  current validation entry points and actual CI coverage.
- [HLint 3.10 function-length implementation](https://github.com/ndmitchell/hlint/blob/v3.10/src/Hint/Smell.hs):
  its physical-span checks and exemptions do not enforce this specification.
- [Fourmolu column-limit documentation](https://fourmolu.github.io/config/column-limit/):
  best-effort wrapping and the need to verify formatting stability.
