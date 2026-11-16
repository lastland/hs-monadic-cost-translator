# Reference-policy specimens

These files are parsed, never compiled or executed. Invalid modules and Cabal
files are deliberate adversarial inputs. Keep them checked in: the policy test
must not manufacture Haskell programs or mutate live semantic fixtures.

`ReferencePolicy` checks fixture layout, imports, compiler hooks and static probe
catalogs. It does not establish semantic correctness or handwritten authorship.
The shared observer and comparator own evaluation, costs, samples and trials.

Cases explicitly export `probes :: [Probe]`. Their finite catalog may use lists,
`map`/`concatMap`/`concat`, comprehensions, tuple tables, pure helper constructors,
and static Boolean branches. A bounded syntax interpreter follows those builders
without evaluating program callbacks or executing Haskell. Labels must resolve
to unique, nonempty static strings. Sample callbacks remain opaque; they cannot
select the catalog or its labels. Unknown or unbounded catalog builders fail.

Successful costed callbacks must reach an actual imported Generated term through
their local definitions and table arguments. Imports, type annotations,
constructors, unrelated snapshots, and failure probes alone do not satisfy that
rule. Generated datatype conversions also exercise generated instances, so those
callbacks may instead use the runtime's `reprM` with a generated datatype type.
Generated-demand probes may reach generated generator/shrinker functions. Every
catalog must still contain a successful probe that reaches a generated function;
failure-only, source-only and constructor-only catalogs fail.

The reachability check does not prove that arbitrary Haskell uses its arguments
or takes a particular branch. The isolated compiler and shared observer remain
responsible for types, import usage, evaluation, outcomes, labels and costs.
Generator-only QuickCheck imports are allowed; runners and trial settings are
not. Explicit positive ReferenceSupport import lists are allowed. Local helpers
may not shadow the shared probe constructors.

The `inventory-*` trees exercise discovery independently of Haskell parsing.
Only the exact diagnostic and fixture-policy lists are infrastructure exceptions.
`Regression.hs` is not an alternative semantic runner or a permitted final file.
