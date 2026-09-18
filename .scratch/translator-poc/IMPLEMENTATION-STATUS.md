# Implementation checkpoint — structural output migration ready for recheck

All seven final-review requests and the structural output migration are
implemented. The full suite passes after the migration.
See `REVIEW-CORRECTIONS.md` for the red-before-green evidence and the semantic
details. Parent/final reviewers own the recheck and commit. No commits or
external-runtime changes were made in this correction session.

## Corrections delivered

1. **Constrained sharing:** different dictionary instantiations get separate,
   stable name subtrees; the review's Int/Integer example now verifies `(1,2)` at
   cost **5**, rather than 4. Same concrete instantiations still share, and
   unconstrained polymorphic parameters do not split identity. Tests cover local
   values, generic callers, multiple constraints, constructor dictionaries and
   nested allocations. Typeable evidence is generated where needed; exact GHC
   type evidence preserves annotations and defaulting without extra source
   annotations.
2. **Names:** encoded operators, identifiers, constructors, types, demand helpers
   and locals have disjoint naming forms. Regressions include `%%` versus
   `operator_37_37`, analogous constructor/type collisions, MagicHash identifiers,
   and record-selector/Prelude collisions.
3. **Record order:** positional extraction is separate from source-ordered
   subpattern tests. `R { b=0, a=0 }` on `R undefined 1` returns 2 without forcing
   a, at cost 2 including the observation wrapper.
4. **Binding diagnostics:** top-level and local pattern-binding failures report
   the declaration location, source pattern and actual limitation.
5. **Composition:** prefix `(.)` and applied/partially applied compositions work
   with generalized named shapes. Captured operands and intermediates retain
   sharing; multi-element map/fold tests independently check exact costs.
6. **Simplification:** driver execution reuses `compileAndRun` returning stdout;
   ReprK/DemandK are constraint synonyms; unused fallback bindings and redundant
   single-constructor/default selector cases are omitted.
7. **Diagnostic phases:** the generated module-name category retains its own
   classification and raw
   candidate, rather than becoming unsupported-source failures.

The earlier higher-kinded implementation remains covered: visible/invisible
kind arguments, Higher/Fix, nested and non-regular recursion, local constructor
mappings, index kinds, bounded generation, compatibility and shrinking.

## Final verification

The output path now constructs GHC `GhcPs` expressions, patterns, types,
statements, declarations, imports and modules directly. `Output.hs` centralizes
GHC constructors and precedence handling; `Pretty.hs` renders the whole module
once. There is no custom output language, source-fragment parser or generated
module reparse. The remaining `parseModule` call parses the input source.
An invalid output-module-name token produces the generated module-name-invalid
diagnostic and retains the candidate. All other candidates continue
through the external GHC compilation/publication gate.

The final review added five fixes, each with a red regression first: Char as
an end-to-end scalar (local identity conversions in each generated module,
since the external runtime has no Char instances), `LitPat` character and
string literal patterns (strings decompose into their character sequence,
preserving cell-by-cell forcing and short-circuiting), composition with
multi-arity right operands (`(map . add) x` supplies the partially applied
`add x` as a thunked callback; no tick for the partial application),
source-located local-signature diagnostics, and the
`GeneratedParseFailure` to `GeneratedModuleNameInvalid` rename. See
`REVIEW-CORRECTIONS.md` section 9 for the evidence.

`test/OutputAstProbe.hs` is retained as standalone feasibility evidence and
included in Cabal's extra source files. It constructs and compiles declarations,
kind signatures, records, conversions, statements and annotations, then checks
the resulting value and cost. The public semantic acceptance tests remain the
regression boundary.

- Focused public regressions were run before the corresponding fixes and again
  after them. The annotation/defaulting and additional namespace regressions
  also passed after their fixes.
- `hlint src app test integration/probes integration/test-integration`:
  **no hints**.
- `stack build`: **passed**, with no translator runtime dependency.
- `stack --stack-yaml integration/stack.yaml test` after the AST migration and
  again after the final review fixes: **all five suites passed**.
  Subsequent HLint-only eta reductions and redundant `$` removal were checked
  with a clean build, HLint and the compiled AST probe.
  - **65** generated fixed-input value/cost cases.
  - **17** CLI diagnostic cases, including pattern binding and local-signature
    locations and the module-name category/publication boundary.
  - A public GHC compilation gate with unused local bindings, overlapping
    patterns and simplifiable class constraints promoted to errors.
  - Generated QuickCheck: **100** trials; first-order and higher-kinded demand
    checks, including **100** bounded infinite-structure trials.
  - Pattern scaling: **169** values/costs, **12,085** generated bytes
    (11,732 before Char support added its local instances).
  - **37** handwritten reference tests, **9** scanl checks, and the independent
    higher-kinded and unconstrained polymorphic-sharing gates.

Each run prints its own fixture directories; fixtures retain generated modules
and compiler/run logs.

## Remaining delivery steps

Parent/final review, any resulting corrections, then the parent's commit.
This records tested proof-of-concept coverage, not a formal proof or support for
all Haskell syntax. The approved exported-constructor requirement remains in
force. Deferred multi-module translation, source deriving and abstract-type
conversion are tracked in issues 01, 02 and 04.
