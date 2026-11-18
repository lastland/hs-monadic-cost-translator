# hs-cost-monadic-translator

## Documentation

Code is the single source of truth for agents. Inspect the implementation and
tests to establish current behavior; documentation is a guide, not the authority.

Write every README, at the repository root and in subdirectories, for human users
to understand the project or directory as it is now: its purpose, setup, usage,
supported behavior, and limitations. Keep development history, fix or improvement
logs, agent context, and task-completion reports out of READMEs. Describe current
behavior directly. Update a README when that human-facing description changes,
not merely because work was completed.

## Tests

Run Stack tests with `sh scripts/stack-test.sh [arguments]` instead of invoking
`stack test` directly.

## Source workflow

- After editing `src/`, run `sh scripts/format-source.sh FILE...`, then
  `sh scripts/check-source.sh` and `sh scripts/check-hlint.sh`. The shared scripts
  own formatting, discovery, tool pins, and source limits.
- After shell changes, run `sh scripts/check-shell.sh`.
- After each coherent extraction, compile and run affected behavioral tests.
  Preserve independent references and inspect changes to generated behavior.
- Before completing substantial work, run `sh scripts/validate.sh` and the full
  Stack reference suite. Report baseline failures separately from regressions.
- Type-annotate new and modified Python code, including tests.
- For tool setup, checker failures, or the opt-in staged hook, read
  `docs/source-policy.md`. Hook installation and Git configuration need approval.

## Agent skills

### Issue tracker

Issues are tracked as local markdown files under `.scratch/<feature>/`. There is
no PR triage surface. See `docs/agents/issue-tracker.md`.

### Triage labels

The five canonical triage roles use their default strings (`needs-triage`,
`needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`).
See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout: one `CONTEXT.md` at the repo root, organized by area.
See `docs/agents/domain.md`.
