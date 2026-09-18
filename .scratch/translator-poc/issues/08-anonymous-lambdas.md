# Anonymous lambdas and capturing closures as callbacks

Status: ready-for-human

## What to build

Design and validate a general translation recipe for anonymous lambdas,
including lambdas that capture surrounding variables, when used as function
arguments. The proof of concept supports named functions and their
compositions as callbacks; the interview deferred lambdas because no general
recipe was established. The motivating example is the Newton-Raphson case
study's `repeatly (next n)`, where the callback both captures a value and is
a partial application.

This is HITL because the function-value representation must be decided
first: how a captured variable's thunk is retained, when the lambda body's
ticks are charged, and how the representation fits each supported
higher-order combinator's calling convention. Start from compiled probes
against the external runtime covering capture, partial application, and
sharing of captured thunks across invocations; record the chosen recipe as
an ADR if a real trade-off is involved.

## Acceptance criteria

- [ ] Compiled runtime probes validate the representation for capturing
      lambdas, with sharing of captured thunks and independently counted
      ticks across multiple invocations.
- [ ] The recipe is recorded and approved before generator work begins.
- [ ] Source examples such as `map (\x -> x + n) xs` and the Newton-Raphson
      callback translate, typecheck, and match hand-calculated costs.
- [ ] Remaining unsupported function-valued forms fail with accurate
      source-located diagnostics.

## Blocked by

None - can start immediately.
