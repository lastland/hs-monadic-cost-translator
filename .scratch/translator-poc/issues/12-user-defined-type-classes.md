# User-defined type classes and instance translation

Status: ready-for-human

## What to build

Design and validate translation of user-defined type classes and their
instances, which the proof of concept currently rejects. Method calls over
thunked representations must preserve demand and sharing, and method
invocation needs a decided tick policy consistent with the fixed cost model
(the current policy gives translated source functions one entry tick;
class methods are user-defined source functions). Dictionary-passing over
generated representations must not force thunked fields prematurely.

This is HITL because the dictionary representation and method tick policy
are design decisions: whether dictionaries are generated as records of
monadic functions, how default methods interact with sharing, and how
overlapping resolution diagnostics are reported. Start from compiled probes
against the external runtime covering a small class with a lazy-field
method; record the chosen design as an ADR.

## Acceptance criteria

- [ ] Compiled runtime probes validate the dictionary representation and
      method tick policy, with independently counted costs.
- [ ] The design is recorded and approved before generator work begins.
- [ ] A small source module with a user-defined class, an instance over a
      user datatype, and method calls translates, typechecks, and matches
      hand-calculated costs.
- [ ] Unsupported class-system features (if any remain) fail with accurate
      source-located diagnostics.

## Blocked by

None - can start immediately.
