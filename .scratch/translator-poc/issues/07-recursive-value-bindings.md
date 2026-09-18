# Recursive value bindings via monadic fixed points

Status: ready-for-human

## What to build

Design and validate a translation for recursive value bindings
(knot-tying definitions) that the proof of concept currently rejects, for
example `ones = 1 : ones`. The paper flags these as requiring investigation
into monadic fixed points; the proof of concept deliberately excludes them
because silently rewriting them into recursive functions changes sharing and
therefore cost.

This is HITL because the sharing semantics must be decided first: the
translated definition must produce one shared cyclic structure whose
repeated observation charges its computation once, matching the source's
sharing rather than re-allocating it per reference. Start from a compiled
probe against the external runtime before any generator changes; record the
chosen semantics as an ADR if a real trade-off is involved.

## Acceptance criteria

- [ ] A compiled runtime probe demonstrates the intended sharing and cost
      semantics for a cyclic definition, with independently counted ticks.
- [ ] The design decision (monadic fix mechanism, naming, sharing identity)
      is recorded and approved before generator work begins.
- [ ] Translated cyclic definitions support finite demands without
      divergence, and repeated observation of the same cycle charges once.
- [ ] Previously rejected recursive-value forms either translate or fail
      with an accurate source-located diagnostic; no silent rewriting into
      recursive functions occurs.

## Blocked by

None - can start immediately.
