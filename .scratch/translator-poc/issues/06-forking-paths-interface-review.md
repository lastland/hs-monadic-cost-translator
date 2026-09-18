# Review "Reasoning about the garden of forking paths" for interface compatibility

Status: ready-for-agent

## What to build

Independently review Li, Xia, and Weirich, *Reasoning about the garden of forking
paths* (2021), DOI 10.1145/3473585, against this project's generated monadic
interface (explicit thunking, forcing, and tick operations over the external
runtime). Produce a compatibility report: where the paper's interface and cost
semantics agree with ours, where they diverge, and which divergences matter for
property-based cost testing.

Record the relevant terminology in the project context docs and correct any
README claims that currently overstate or understate compatibility. The PRD
notes this review as a standing obligation before claiming detailed
compatibility. This is a research slice: no translator code changes are in
scope. If the review surfaces concrete interface changes worth making, file
them as separate issues rather than implementing here.

## Acceptance criteria

- [ ] A written report covers the paper's monadic interface, thunk/cost model,
      and property-testing approach, each compared against this project's
      generated interface with cited sections.
- [ ] Divergences are classified: terminology-only, cost-model difference, or
      incompatibility affecting generated code.
- [ ] README and context docs are updated to reflect only verified claims.
- [ ] Any proposed follow-up work is filed as new issues, not implemented.

## Blocked by

None - can start immediately.
