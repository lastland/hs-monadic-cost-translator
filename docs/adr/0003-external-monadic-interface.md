# Generate against an external monadic interface

Generate a separate readable module against the existing thunk, annotation,
demand, and testing interfaces exemplified by `hs-pbt`. Reuse its handwritten
source-program higher-order implementations initially, rather than require a
general translation of their definitions. This prioritizes usable cost-testing
translations while exposing a documented boundary of supported library bindings.

Do not bundle `hs-pbt` into the `cost-translate` package or declare `MemoristANT`
as a build dependency of the translator library or executable. Nevertheless, GHC
must typecheck generated code against the actual target interface before success
is reported. Use GHC's existing environment and report unavailable modules as
setup errors; do not add project discovery, automatic installation, or a custom
runtime-location mechanism.

For reference and integration tests, both Stack projects register the
[pinned `hs-pbt/` submodule](../runtime.md). This gives GHC the
actual runtime when it compiles generated modules and their callers. Parsing
generated text alone is not sufficient validation. Users write their own
QuickCheck properties; the translator generates representation and structural
demand support, not cost specifications.
