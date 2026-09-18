# Explicit strictness: bang patterns, strict fields, seq, and strict application

Status: ready-for-agent

## What to build

Support the explicit strictness features the proof of concept currently
rejects: bang patterns, strict constructor fields, the `Strict` and
`StrictData` extensions, `seq`, and strict application `$!`. Each translates
to an explicit forcing operation at the point the source demands it, so
evaluation order matches the source while the fixed tick policy is
unchanged: forcing an operand retains the cost of computing it, and the
force operation itself adds no tick.

## Acceptance criteria

- [ ] `f !x = 0` forces `x` even though unused; `f undefined` fails as in
      the source.
- [ ] Strict fields force on construction; ordinary lazy fields remain
      thunked.
- [ ] `seq` and `$!` translate to explicit forcing with source-equivalent
      results and independently verified costs.
- [ ] Unticked primitives on strict types remain unticked; only the
      operand's own computation cost is charged.
- [ ] README limitations and cost-policy sections updated.

## Blocked by

None - can start immediately.
