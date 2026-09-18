# Newtype support with a demand-preserving translation rule

Status: ready-for-agent

## What to build

Support `newtype` declarations, which the proof of concept currently
rejects. The rule must be demand-preserving: matching a `newtype`
constructor must not introduce the constructor-forcing step that ordinary
`data` translation performs, because in the source a `newtype` wrapper adds
no evaluation. Field forcing, sharing, and cost behavior must match the
underlying representation exactly, with no additional ticks for the wrapper.

## Acceptance criteria

- [ ] `newtype` declarations generate representation, conversion, and
      demand support matching their underlying type's structure.
- [ ] Pattern matching on a `newtype` constructor does not force beyond
      what matching the underlying value would force.
- [ ] Values and costs match the equivalent unwrapped code on the same
      demands, verified against hand-calculated costs.
- [ ] Partially applied newtype constructors used as functions behave like
      their underlying conversion, or fail with an accurate diagnostic.
- [ ] README limitations updated.

## Blocked by

None - can start immediately.
