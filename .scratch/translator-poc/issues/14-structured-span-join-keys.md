# Structured source-span join keys

Status: ready-for-agent

## What to build

Replace the location-string map keys that join renamed and typechecked
trees with structured source-span keys. The final standards review flagged
the current string keys as fragile: span collisions are patched with ad hoc
rules, and string rendering of spans is an inexact join between compiler
passes. An exact structured span key removes the ad hoc collision handling
without changing any translated behavior.

This is an internal-correctness refactor: the public acceptance boundary
(all generated value/cost, diagnostic, and scaling suites) must pass
unchanged.

## Acceptance criteria

- [ ] Span-keyed joins use a structured key type end-to-end; the
      location-string rendering join is gone.
- [ ] Ad hoc span-collision special cases are removed or replaced by
      explicit, tested disambiguation.
- [ ] Full integration suite passes with identical results; no generated
      output changes on the acceptance fixtures.
- [ ] HLint clean; build clean.

## Blocked by

None - can start immediately.
