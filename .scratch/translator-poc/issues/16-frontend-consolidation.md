# Frontend consolidation: environment, builtin dispatch, signature rendering

Status: ready-for-agent

## What to build

Consolidate the three duplications the final standards review flagged in
the translation frontend: the eight-field environment record threaded
through every emitter (split so resolution, literal handling, and type
evidence change independently), the builtin-dispatch repeated across
pattern matching, expression emission, and name resolution (one shared
table), and the signature rendering computed three ways with differing
flag plumbing (one path).

This is a behavior-preserving cleanup: the public acceptance boundary must
pass unchanged. Do it after the span-key refactor so the join handling it
touches is already structured.

## Acceptance criteria

- [ ] Builtin dispatch for `[]`, `True`, `False`, `(:)`, and `(.)` comes
      from one shared definition.
- [ ] Signature rendering has a single implementation used by top-level,
      local, and validated-signature paths.
- [ ] The environment record is split along the responsibilities the
      review identified, with emitters taking only the pieces they use.
- [ ] Full integration suite passes unchanged; HLint clean; no generated
      output changes on the acceptance fixtures.

## Blocked by

- `.scratch/translator-poc/issues/14-structured-span-join-keys.md`
