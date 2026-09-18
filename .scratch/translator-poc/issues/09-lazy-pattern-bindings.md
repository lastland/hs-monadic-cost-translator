# Lazy pattern bindings

Status: ready-for-agent

## What to build

Support lazy destructuring patterns: `let (x, y) = p` bindings and explicit
irrefutable `~` patterns, which the proof of concept currently rejects. The
translation must preserve their defining property: the pattern match does
not force the scrutinee until a bound component is demanded. Translate the
whole binding into a suspended value and select components on demand, so
`ignorePair p = let (x, y) = p in 0` returns `0` even when `p` is bottom,
while demanding `x` forces `p` exactly once with sharing preserved.

## Acceptance criteria

- [ ] `let`/`where` tuple and constructor patterns translate without forcing
      the bound value until a component is demanded.
- [ ] Explicit `~` patterns translate with the same demand behavior.
- [ ] A demanded component forces the suspended binding once; repeated
      observation of the same binding charges its computation once.
- [ ] Demanded failing matches remain ordinary runtime errors, and unused
      failing matches remain suspended.
- [ ] Unsupported lazy-pattern shapes (if any remain) fail with accurate
      source-located diagnostics; README limitations updated.

## Blocked by

None - can start immediately.
