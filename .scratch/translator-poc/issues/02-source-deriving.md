# Source deriving support

Status: needs-triage

## Follow-up requested by the PRD

Design translation of source deriving clauses, including the semantics and cost
policy of derived methods over thunked representations. Keep this separate from
the generated demand types' own Eq/Show instances.

Source deriving clauses currently fail whole-module translation. Do not accept
them by silently dropping their methods or applying structural operations to
runtime thunk metadata.
