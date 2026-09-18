# Current generated-code conventions

This file replaces the scaffold's unapproved restrictions. The PRD remains the
scope authority. See `IMPLEMENTATION-STATUS.md` for final verification; the
higher-kinded audit regressions now pass.

- Generate from GHC-renamed syntax after source typechecking, before optimization.
- Build all output expressions, patterns, types, statements and declarations as
  GHC `GhcPs` syntax through `Output`. Names and literal payloads are atomic
  strings; Haskell source fragments are not an emission interface. `Pretty`
  renders the completed module once, with the centralized language pragma.
  Do not parse generated fragments or reparse a string-built module.
- Name translated identifier functions `nameM`; encode symbolic names into valid
  identifiers. Log resolved source identities and arity/adaptation decisions.
- Function parameters are thunks; calls return `MemoristM`. One entry tick per
  translated source function, including local helpers and record selectors.
- Suspend argument and constructor-field actions. Never sequence a lazy value's
  body merely to allocate its binding.
- Use source-ordered pattern/guard failure continuations. Fresh case/guard thunk
  names must avoid all retained source bindings, including field names.
- Share failure continuations with ordinary let-bound monadic actions, never
  by duplicating their syntax trees or allocating/ticking a thunk for them.
- Reserve one anchor per shared source value. Unconstrained polymorphism keeps
  that identity across instantiations. Generalized constrained variables select
  stable, prefix-free subtrees by structural TypeRep keys; unrelated result
  parameters are not keys. A pure unit-argument factory retains constrained
  generalization without reallocating a name on each reference. This uses the
  external runtime's `lazy`/`eval` operations and existing named-thunk structure,
  with no unsafe coercion or external-runtime API change.
- Modules with top-level values export `Bindings` and `initialize`. Reusing one
  initialized record shares values across exported calls in that evaluation.
- Named partial applications retain supplied thunks. Composition suspends the
  intermediate result. Applying the first input to a composition captures that
  intermediate once, even when arguments remain; nested compositions preserve
  the same rule. Prefix and infix spellings share one callable representation.
  Neither adapter adds ticks.
- A source Foldable constraint becomes the approved direct FoldableT interface.
  Lists delegate to handwritten folds; Maybe/pairs have direct ticked equations.
  Log maximal applications only, not intermediate HsApp nodes.
- Generate `TypeT`, `TypeD`, source conversion instances and runtime demand
  instances. Lists, pairs and Maybe reuse existing representations.
- Require source datatypes and all their constructors to be exported for
  generated conversions (user-approved). Validate resolved export identities
  and report hidden declarations at their source locations before emission.
- Generate result-indexed finite-budget demand generators and compatible
  shrinkers; no regular-recursion restriction is imposed. List/pair nested
  parameter changes and higher-kinded non-regular recursion have passed
  generated-code tests.
- Preserve kind binders but omit invisible kind arguments from ordinary type
  applications. Generate kind-indexed RepOf/DemOf mappings and quantified
  application constraints over the existing runtime classes. Explicit dictionary
  witnesses supply recursive mapping equalities without allocating thunks or
  ticking. Eq/Show/Arbitrary demand dictionaries use the shared ShapeK lifting
  scheme; bounded generation uses GeneratedK.
- Pretty-print with GHC, then typecheck the candidate against the existing
  runtime environment before publishing it. Keep failed candidates in the log.
- Inspect desugared types only to validate the strict-scalar primitive boundary.
  Also retain exact GHC type evidence at variable uses and explicit annotations,
  without reconstructing GHC wrapper rules. Keep concrete numeric literal types;
  instrument renamed syntax.
- Record patterns extract positional fields but test them in source order.
- Operator encodings and ordinary identifiers occupy disjoint generated-name
  forms. Local generated names and demand helpers have separate forms too.
- Preserve diagnostic phase information. Generated parser failures are not
  unsupported-source failures; retain their rejected candidate in the log.

The original handwritten P5 probes initialize a value within a single wrapper;
they do not establish cross-exported-call sharing. The generated-code tests and
the polymorphic-sharing gate now cover that separate obligation.
