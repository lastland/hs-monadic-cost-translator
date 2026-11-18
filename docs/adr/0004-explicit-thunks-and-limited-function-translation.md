# Preserve explicit sharing with limited function translation

Represent shared value bindings with shared thunks and wrap every ordinary lazy
datatype field in a thunk, including primitive-valued fields. Primitive values
have no internal lazy structure, but computing a field can still diverge or fail.
Reusing runtime usage-set accounting only works if the translator preserves thunk
identity rather than reallocating a binding at each reference.

Module-level zero-argument values are currently emitted as ordinary monadic
actions so every source function remains directly callable without an initializer
or environment argument. Executing those actions again does not retain their
earlier results. Source-global sharing costs and thunk identity are a temporarily
accepted Memorist limitation tracked in
[issue #2](https://github.com/lastland/hs-monadic-cost-translator/issues/2), with
the identity scope confirmed in
[issue #4](https://github.com/lastland/hs-monadic-cost-translator/issues/4). This
does not change the source cost model or weaken local sharing or local identities.

For function applications, use resolved definitions, inferred types, explicit
parameter counts, supplied argument counts, and supported library conventions.
Log syntactic counts and adaptations. Do not require annotations or arity files:
for this subset they duplicate information already present in the definition.

Support named local and top-level functions, captured surrounding variables,
composition, and direct partial application at arbitrary supported argument
counts. The binary `curryKM` adapter is a reference pattern, not a two-argument
scope limit. Its generalized use must be validated end to end before generating
it automatically. Reject unsupported aliases and computed function results
rather than infer calling behavior through arbitrary code or accept unchecked
numeric overrides that could move tick boundaries.

This deliberately limits analysis while retaining useful higher-order library
calls. Recursive value bindings and anonymous source lambdas remain deferred;
local named functions capturing values are explicitly supported.
