# Source checks

The shared scripts check formatting, source limits, shell scripts, and Haskell
lint. Local validation, the optional Git hook, and CI use the same checks.

## Tools and configuration

Install Python 3 and the exact GHC, Fourmolu, HLint, and ShellCheck versions in
[`scripts/source-policy.json`](../scripts/source-policy.json). Full validation
also needs Cabal and Git for the hook regressions. Standalone staged checks need
Git; the shared working-tree checks do not need Git metadata. The scripts do not
install tools; missing tools and version mismatches fail rather than skip a check.

Set `GHC`, `FOURMOLU`, `HLINT`, or `SHELLCHECK` to select an executable. Use an
absolute executable path when running staged checks, because those checks run
inside a temporary source tree. The selected versions must still match the pins.

Repository configuration lives in `fourmolu.yaml`, `.hlint.yaml`, and
`.shellcheckrc`. The shared JSON contains the source limits and shell-discovery
exclusions. Change the shared configuration rather than adding independent limits
to hooks or workflows.

## Everyday commands

Run these commands from the checkout:

```sh
# Format selected implementation files, or omit paths to format all of src/.
sh scripts/format-source.sh src/Cost/Translate.hs

# Read-only checks of the complete working tree.
sh scripts/check-source.sh
sh scripts/check-shell.sh
sh scripts/check-hlint.sh

# Check the compiler without requiring the other validation tools.
sh scripts/validate.sh --check-compiler
sh scripts/validate.sh --print-compiler

# Aggregate checks, regressions, warning-clean build, and runtime-free tests.
sh scripts/validate.sh
```

`check-source.sh` first requires Fourmolu-clean input. Only then does it measure
the complete `src/` tree, including untracked files and hidden entries. Formatting
failures stop metrics; read-only checks never rewrite files. The six limits cover
code-bearing file lines, module-level value definitions, definition spans,
physical line width, decoded string-literal occurrences, and immediate directory
entries. They do not apply outside `src/`.

The metrics use GHC's lexer and parser with GHC2021 and each file's language
pragmas. Invalid encoding, syntax failures, symlinks, missing source roots, and
unsupported source inputs are errors, not exemptions. CPP/preprocessing and
unsupported source kinds require an explicit policy before they can be accepted.

`check-shell.sh` discovers maintained `.sh` files recursively, including
`test/generated-functions/PublishMissing.sh`, and the extensionless
`.githooks/pre-commit` hook.
It excludes paths with components named in the JSON `shell_exclude` list:
Git metadata, scratch work, Stack/Cabal build output, and the external `hs-pbt`
dependency. New maintained shell files are included without updating a file list.
Scripts are checked according to their declared shell.

`check-hlint.sh` retains suggestions and the `src`, `app`, `test/*.hs`, and
`test/reference/*.hs` coverage. Fixture and mutation subdirectories are not added
to that HLint scope.

## Optional staged checks

Run the hook checks without installing anything:

```sh
sh scripts/check-staged.sh
# The thin hook can also be invoked directly from the checkout.
.githooks/pre-commit
```

These commands check the complete Git index, not changed lines or working-tree
contents. They export raw staged blobs into a temporary tree and run only the
shared source, shell, and HLint checks there. Staged scripts, checker sources, and
configuration are used too. Stage the required infrastructure before invoking
the hook; an unstaged replacement cannot repair missing staged infrastructure.

One frozen index supplies both staged bootstrap programs and the checked tree.
The effective `GIT_INDEX_FILE` is honored, including relative or absolute
alternate index paths and split indexes. If another process changes that index
during checks, the commands fail without reverting the other process's changes.
Rerun the checks against the new index.

Untracked files and unstaged edits do not affect staged checks. Intent-to-add
placeholders have no staged content; a required bootstrap file with an empty
staged payload is an error. Ordinary working-tree checks still inspect untracked
files in their scopes. Neither command formats, stages, stashes, installs hooks,
changes Git configuration, or modifies the index and working files.

The Python bootstrap runs in isolated mode. Snapshot checks discard inherited
Python import configuration and disable user-site imports, so untracked checkout
modules cannot replace standard modules. `PATH` and the explicit GHC, formatter,
and linter executable overrides remain available. Git replacement objects are
disabled: checks read the actual indexed blobs, not their replacement refs.

The staged tree must contain only ordinary files and directories, except for the
canonical `hs-pbt` submodule gitlink (`160000`), which is omitted from the
exported snapshot because external runtime sources are outside the staged
source, shell, and HLint checks. Symlinks, any other submodule or gitlink path,
unresolved index conflicts, non-UTF-8 paths, and paths that cannot be
represented distinctly on the local filesystem are rejected explicitly. The
export does not apply checkout filters, line-ending conversion, or
`export-ignore` attributes.

To opt in to automatic pre-commit checks, first inspect and record any existing
hook setup. Only if you intend to select this repository's hook directory, run:

```sh
git config --get core.hooksPath
git config --local core.hooksPath .githooks
```

The first command may report no value. The second changes local Git
configuration and replaces the selected hook directory; do not run it blindly
when another hook setup is in use. Restore your recorded setting to undo the
change. No hook manager or pre-push hook is required.

## Regressions and CI

Reference selection checks every known-failure record against an existing local
issue in `.scratch/reference-failures/issues/`. Cabal packages those issue files
as data alongside the fixtures, so installed and source-distribution tests resolve
them through the package data directory, not the current working directory.
Missing issue data makes discovery fail before listing or running references.
The `test-selection` and `fixture-policy` groups do not need the external runtime.

```sh
python3 test/source_policy.py --ghc /path/to/ghc --self-test
python3 test/staged_policy.py --ghc /path/to/ghc --self-test
python3 test/compiler_preflight.py
```

The staged regressions create disposable repositories and exercise the actual
hook and shared commands with installed pinned tools. They verify staged versus
unstaged behavior and check that the index and working files remain unchanged.

CI runs on every push and pull request without path filters. The runtime-free
job runs compiler preflight and `sh scripts/validate.sh`, then checks the
committed `examples/*M.hs` translations against fresh generator output.

Two independent Stack runs build the [pinned `hs-pbt/` submodule](runtime.md).
One runs passing handwritten references plus infrastructure checks; the other
runs all four integration suites. To reproduce them locally after setting up the
runtime:

```sh
sh scripts/stack-test.sh
sh scripts/stack-test.sh --stack-yaml integration/stack.yaml cost-translate-integration
```

The default reference run excludes the explicit known-failure catalog. To run
the complete reference inventory, including those cases with unchanged assertions:

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments=--all-references
```

This command continues after ordinary case failures and exits nonzero while any
selected reference fails. See the [testing guide](../README.md#testing-and-validation)
for known-failure-only runs, listings and individual source selectors.
