# Final review corrections

All seven requested corrections were implemented with public-boundary
regressions. No commit or external runtime edit was made.

## 1. Dictionary-sensitive sharing

The reported overflow example first reproduced value agreement with cost 4.
It now checks both values `(1,2)` and cost **5** through one initialized Bindings
record. The stable name key uses only generalized variables occurring in source
dictionary predicates, selected from the inferred result through type-level
projections. It does not use all result parameters, a hash, a dictionary pointer,
or a fresh name on each reference.

`Typeable` evidence is propagated through affected generated signatures/callers.
Canonical keys encode qualified tycon identity, kind arguments and applications.
Negative delimiters reserve disjoint, prefix-free subtrees under each binding's
anchor; ordinary allocator components are nonnegative. Entire nested evaluations
use the selected subtree, so both root and descendant costs stay distinct.

The regressions cover same-dictionary reuse, unconstrained result parameters,
unconstrained polymorphic sharing, local constrained values, generic callers,
constructor dictionaries, multiple constrained variables, nested allocations,
ignored/scoped annotations and source defaulting. Exact GHC expression type
evidence and explicit binding signatures prevent the extra evidence requirements
from discarding a source-selected instantiation.

## 2. Injective names

The `%%` / `operator_37_37` collision failed generated typechecking before the
fix and now executes with the independently expected value/cost. Symbolic names
end in character codes, while ordinary translated identifiers end in M/T/D.
Constructor representation and demand encodings use separate prefixes. Demand
helper names separate encoded operators from ordinary datatype names. Local
identifiers end in an underscore, outside public generated naming forms.

Additional regressions cover constructor collisions, type/demand-helper
collisions, helper-shaped source names, MagicHash names, and a record selector
whose generated name conflicts with Prelude.mapM. Selectors are included in
Prelude hiding.

## 3. Record subpattern order

`R { b=0, a=0 }` previously forced an undefined `a` before observing the mismatch
in `b`. Extraction now assigns slots by constructor layout, and the matcher
visits supplied fields in source order. The review example returns **2**, cost
**2** including its observation wrapper.

## 4. Binding diagnostics

Both top-level and local pattern-binding cases failed with unlocated generic
messages before the fix. They now report the declaration span, the source
pattern (without internal unique suffixes), and the named-binding limitation.

## 5. Composition and partial application

Both reported forms first failed translation. Prefix and infix composition now
use the same callable preparation/specialization logic. Captured argument
thunks are allocated once when preparing the function value. Supplying its
first argument captures the intermediate thunk before returning any remaining
partial function. This matters beyond accepting the syntax: two map elements
must not recompute the captured `twice x`.

Exact-cost regressions cover partial map callbacks, prefix `(.)`, four-argument
named shapes, nested partial compositions and separately captured operands.
The two-element partial-map case yields 30 at cost 12; the nested case yields
50 at cost 13. Adapters add no ticks.

## 6. Simplifications

- All generated driver compile/execute paths reuse `compileAndRun`, which now
  returns stdout while preserving compiler/run logs.
- ReprK/DemandK forwarding classes became constraint synonyms. The complete
  higher-kinded fixture and independent runtime gate pass with those synonyms.
- Single unguarded RHSs and last equations omit unused fallback bindings.
- One-constructor pattern matches, total selectors, demand usage and compatible
  shrinking omit redundant wildcard clauses.
- A public GHC check with `-Werror=unused-local-binds`,
  `-Werror=overlapping-patterns` and `-Werror=simplifiable-class-constraints`
  first failed and now passes. This is not an assertion about emitted AST text.

## 7. Diagnostic phases

The invalid generated module-name regression reproduced the blanket
unsupported-source classification. A two-case Diagnostic type now preserves
UnsupportedSource versus the invalid-module-name category at the
frontend/publication boundary. The rejected candidate is retained, the command
exits unsuccessfully, and nothing is published. After the structural migration,
`Pretty` validates the module-name token directly instead of reparsing
generated source. The same public diagnostic regression passes. Existing
source, setup and generated type-error handling remains at its respective
phase.

## 8. Structural output follow-up

All output emitters now construct native GHC `GhcPs` syntax through `Output`.
`Pretty` renders the completed module once. The compiled builder probe and the
full public acceptance suite pass; the earlier semantic corrections remain
covered. See `IMPLEMENTATION-STATUS.md` for the latest verification sequence and
`SIZE-READABILITY-REVIEW.md` for measured implementation/output sizes.

## 9. Final review fixes

Five substantiated findings, each red before its fix and green after:

1. **Char scalar:** `data B = B Char; f (B c) = 5` previously failed as a
   generated-code type error. Char is now a scalar end-to-end: the `Scalar`
   instance and comparison validation admit it, generated demand generation
   includes it, char literals work in expressions and comparisons, and each
   generated module carries local identity conversions (`Rep Char = Char`,
   `Demand`/`LBoundDemand`/`DemandArb` counterparts) because the external
   runtime has no Char instances and must not change. The runtime keeps
   plain `Char` fields, exactly like its own Int/Bool scalars.
   Regression: `char-scalar-field` (value 5, cost 2).
2. **Literal patterns:** only overloaded numeric `NPat` was implemented;
   `f 'a' = ...` and `f "ab" = ...` failed as unimplemented patterns. `LitPat`
   now supports characters (forced-thunk equality like numeric patterns) and
   strings, where the literal decomposes into its `ConsT`/`NilT` character
   sequence so forcing proceeds cell by cell and stops at the first mismatch.
   Both the desugarer check (char patterns desugar to case matches, string
   patterns to the dictionary-free `eqString`) and the comparison validator
   leave these paths unaffected. Regression: `literal-pattern-char-string`
   (value 14, cost 4, includes source `==` on Char).
3. **Composition with multi-arity right operand:** `f . g` rejected any `g`
   with resolved arity above one. The check now requires a positive arity
   only. The first input flows through `g` once and is captured exactly once;
   a saturated `g` suspends its data result as a thunk, while a partially
   applied `g` suspends its function value for the left operand's convention
   (for example the thunked callback `mapMM` expects). Partial application of
   such compositions works; `add` ticks once per demanded element and the
   partial application itself adds no tick. Regression:
   `composition-arity-two-right-operand` (`(map . add) x`, value 10, cost 6,
   hand-verified tick by tick against the runtime's `mapMM`/`headM`).
4. **Local-signature diagnostics:** `translatedSignatureUsing` and the local
   `sharingPaths` lookup returned bare messages, so "function-valued result
   has no validated calling convention", "unsupported type constraint",
   "unsupported kind" and "cannot locate constrained type parameter" could
   reach users without a file:line, violating ADR-0002. All three emission
   sites (top-level value signatures, explicit local signatures, local shared
   paths) now prefix the signature's or binding's source span. Regression:
   CLI `local-signature-constraint` asserts the location and message on one
   line. (The sharingPaths wrap is defensive: GHC's ambiguity check keeps
   that particular error unreachable without higher-rank types.)
5. **Stale diagnostic name:** `GeneratedParseFailure` described a reparse
   that no longer exists; the constructor is now `GeneratedModuleNameInvalid`
   with matching message text, because post-AST `prettyModule` only validates
   the module-name token. The public regression asserts the new category and
   the retained candidate.

## Verification

Focused tests/typechecks and HLint ran throughout. The final `stack build`,
requested HLint command (no hints), and the full integration suite all passed.
The full suite was also rerun after the AST migration and after the final
review fixes. It includes 65 fixed-input generated value/cost cases, 17 CLI
diagnostic cases, generated property/demand checks, the strict-warning
compilation gate, 37 handwritten reference tests, 9 scanl checks, and both
runtime feasibility gates.

Parent/final reviewers will recheck these changes before committing.
