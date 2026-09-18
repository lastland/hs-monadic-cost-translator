# Configurable tick instrumentation policies

Status: ready-for-human

## What to build

Design and validate selectable tick policies beyond the fixed default, so a
source module can be instrumented under a different declared cost model.
The motivating case is the paper's sorting case study, which counts
comparisons instead of function invocations: ticks attach to comparison
operations and ordinary function-entry ticks are omitted. The default
policy must remain exactly as documented and unchanged.

This is HITL because the selection mechanism and semantics are design
decisions: how a user declares the policy for a translation run, whether
policies compose (entry ticks plus comparison ticks), and how the generated
library-function instrumentation interacts with a non-default policy. The
paper's handwritten comparison-counting sorts serve as the reference
oracles. Record the chosen design as an ADR.

## Acceptance criteria

- [ ] The policy declaration mechanism is designed and approved before
      implementation.
- [ ] Under the comparison-counting policy, translated insertion and merge
      sorts match the handwritten reference costs on the same inputs and
      demands.
- [ ] The default policy is byte-for-byte unchanged for existing accepted
      modules; existing cost regressions all pass.
- [ ] Policy selection and its cost-model meaning are documented in the
      README, including which library instrumentation changes per policy.

## Blocked by

None - can start immediately.
