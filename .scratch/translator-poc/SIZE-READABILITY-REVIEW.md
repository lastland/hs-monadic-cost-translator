# Final size/readability review

This is a manual review against the supplied ruthless-analysis findings. No
task/subagent tool was exposed; this does not claim an independent named-agent
review. Parent retains the final standards/spec review.

## High-impact findings addressed

1. **Exponential fallback duplication:** failure branches share let-bound
   monadic actions, with no new thunk or tick. The ten-equation acceptance case
   is now 12,085 bytes instead of 2,030,630, and all 169 values/costs agree.
   It was 11,732 after the structural output migration and 13,836 before it;
   Char support's local identity instances account for the later increase.
2. **Phantom adapter logs:** maximal applications are logged once. The regression
   rejects intermediate AST-spine adapters that do not occur in source.
3. **Obsolete acceptance substitute:** TestSupport and P8's bytecode loader were
   removed. Actual subprocess translation/typecheck/execution tests remain.
4. **Repeated lazy application rendering:** ordinary calls, constructors,
   records, pairs, lists and operators share `lazyArguments`. The special `+`
   path and duplicated sharedAt declaration branches were removed.
5. **Demand infrastructure:** retained the result-indexed interface and budget
   discipline; reused the runtime ThD `liftArbitrary`/`liftShrink` methods after
   budget decisions.
6. **Higher-kinded emitter assumptions:** fixed rather than declared out of
   scope. A compiled independent probe validates the kind-indexed lifting scheme
   over the existing runtime. One shared ShapeK scheme handles demand Eq, Show
   and Arbitrary; there is no parallel family of arity-specific generators.
7. **Final semantic review:** constrained sharing, name injectivity, record test
   order, applied composition and diagnostic phase bugs now have passing public
   regressions. Details and their observed failures are in REVIEW-CORRECTIONS.md.
8. **Structural output:** every emitter now returns GHC `GhcPs` syntax. The
   completed module is rendered once in `Pretty`; no generated fragments or
   assembled modules are reparsed. This includes kinds, dictionary-sharing
   support, datatype/demand methods, folds, callable adapters and scalar support.

## Current structure and tradeoffs

- `Frontend.hs`: 608 lines. It still combines GHC setup, signatures, logging and
  structural emission. Ordered syntax emission is the main complexity; extraction
  solely to lower its line count would not remove that complexity.
- `Datatypes.hs`: 264 lines. The added correctness obligations are
  real, but emission is now split into `Constructor` information, application
  witnesses and named per-method renderers. Review no longer requires following
  one large interleaved declaration template.
- `Kinds.hs`: 130 lines. ReprK/DemandK are now constraint synonyms, removing the
  redundant forwarding dictionaries. Its generator emits the same kind-recursive dictionary
  patterns for representation and demand mappings, with a shared generic shape
  lift. Standalone kind signatures and explicit application witnesses were
  required by compiled probes, not added speculatively. The bridge is omitted
  entirely when there are no source datatypes.
- `Output.hs`: 259 lines of builders and aliases for native GHC syntax, including
  precedence, locations and extension fields. It defines no parallel output
  language and performs no parsing or rendering. This is the main added
  maintenance cost, tied to the pinned GHC version.
- `Pretty.hs`: 39 lines. It renders once and centralizes output extensions and
  pragmas. Module-name token validation preserves the invalid-basename diagnostic
  because GHC's `ModuleName` constructor itself accepts arbitrary strings.
- `Names.hs`: 30 lines. Symbol encodings, ordinary names and demand helpers now
  use disjoint forms; local names and Prelude selector hiding are audited too.
- `Callable.hs`: 103 lines. This retains the structure needed to capture a
  composition's intermediate once at partial application, instead of flattening
  prematurely into a lambda that reallocates it on every invocation.
- `Sharing.hs`: 91 lines. Type-level projections avoid reflecting unrelated
  result parameters. Qualified structural type keys avoid hash/short-name
  collisions and stable subtrees preserve sharing, including nested allocations.
  The reflection helper is omitted from modules that do not need it.
- `Typed.hs`: 35 lines. It asks GHC for exact type evidence at uses/annotations;
  it does not recreate the compiler's wrapper/subsumption rules.
- `Diagnostic.hs`: 10 lines. A two-case boundary type distinguishes unsupported
  source from generated parsing failure; no general diagnostics framework was added.
- `test/Main.hs`: 610 lines. Compilation/execution now consistently reuses
  `compileAndRun`, returning stdout and retaining logs. Tests remain at the
  agreed public seam. Higher-kinded coverage
  includes exact costs, conversion values, non-Type indices, finite-budget
  termination and compatibility rather than snapshots of emitted class text.
- `RuntimeHigherKindedGate.hs`: 208 lines. This is independent feasibility
  evidence, not another runtime or a test coupled to output formatting. It is
  compiled and executed by the integration project.
- The 683-line handwritten Probes module mostly records independent derivations
  and reference programs. Its length alone does not justify deleting evidence.

### Measured size tradeoff

`wc -l src/Cost/Translate.hs src/Cost/Translate/*.hs app/*.hs` totals **1,715
production lines**, including comments and blank lines. The seven previously
recorded emitter/name module counts above increased by a combined **50 lines**
(Char support, literal patterns and signature locations); `Output.hs` had
already added **259** more. That comparison excludes modules whose earlier
counts were not recorded, so it is not a whole-tree diff. The implementation is
larger, while syntax construction is explicit and punctuation/layout decisions
are centralized. The 59-line standalone builder probe is separate evidence;
the 625-line public acceptance driver grew by the five final-review
regressions.

The pattern-scaling output is **354 bytes larger than the post-migration
baseline** (12,085 versus 11,732) because Char support adds fixed local
identity instances to every generated module; it remains far below the
2,030,630-byte figure before shared failure actions. This measures one
fixture, not a general compression guarantee.

## Semantic review outcomes

- Structured runtime Eq can compare thunk metadata. The source strict-scalar
  policy is validated, and inferred literal types are retained through emission.
- Private constructors are now the approved explicit unsupported case, checked
  at source locations before candidate emission. Future support is issue 04.
- Higher-kinded parameters are supported with kind-correct mappings and
  quantified application dictionaries. Source fields stay thunked; witnesses
  add zero ticks. The former kind errors no longer block delivery.
- Non-regular `Mixed f a = Leaf a | Branch (f (Mixed f [a]))` conversion and
  structural demands pass. No regular-datatype restriction was introduced.

HLint reports no hints. The final full integration suite passes, including a
public strict-warning compilation gate for unused fallbacks, redundant patterns
and simplifiable constraints. Parent/final reviewers will recheck these changes;
this manual review is not a formal correctness proof.
