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
