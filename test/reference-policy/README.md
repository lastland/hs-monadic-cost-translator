# Reference-policy specimens

These files are parsed, never compiled or executed. Invalid modules and Cabal
files are deliberate adversarial inputs. The policy tests use these checked-in
specimens rather than generating Haskell programs or mutating live semantic
fixtures.

`ReferencePolicy` checks fixture layout, imports, compiler hooks and static probe
catalogs. It does not establish semantic correctness or handwritten authorship.
The shared observer and comparator own evaluation, costs, samples and trials.

Cases explicitly export `probes :: [Probe]`. Their finite catalog may use lists,
`map`/`concatMap`/`concat`, comprehensions, tuple tables, pure helper constructors,
and static Boolean branches. A bounded syntax interpreter follows those builders
without evaluating program callbacks or executing Haskell. Labels must resolve
to unique, nonempty static strings. Sample callbacks remain opaque; they cannot
select the catalog or its labels. Unknown or unbounded catalog builders fail.

Successful costed callbacks must reach an exported `Generated` function, an
exported `Generated` constructor (applied as a value or record construction), or
the runtime's `reprM` with an exported `Generated` datatype type through their
local definitions and table arguments. Generated-demand probes may reach
generated generator/shrinker functions. Imports, type annotations, pattern-only
constructors, private or non-`Generated` constructors, unrelated snapshots, and
failure probes alone do not satisfy that rule; failure-only, snapshot-only, and
source-only catalogs fail.

The reachability check does not prove that arbitrary Haskell uses its arguments
or takes a particular branch. The isolated compiler and shared observer remain
responsible for types, import usage, evaluation, outcomes, labels and costs.
Generator-only QuickCheck imports are allowed; runners and trial settings are
not. Explicit positive ReferenceSupport import lists are allowed. Local helpers
may not shadow the shared probe constructors.

The `inventory-*` trees exercise discovery independently of Haskell parsing.
Only the exact diagnostic and fixture-policy lists are infrastructure exceptions.
`Regression.hs` is not an alternative semantic runner or a permitted final file.
