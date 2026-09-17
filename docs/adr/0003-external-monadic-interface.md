# Generate against an external monadic interface

Generate a separate readable module against the existing thunk, annotation,
demand, and testing interfaces exemplified by `hs-pbt`. Reuse its handwritten
source-program higher-order implementations initially, rather than require a
general translation of their definitions. This prioritizes usable cost-testing
translations while exposing a documented boundary of supported library bindings.

Do not bundle `hs-pbt` or declare it as a dependency of the translator package.
Nevertheless, GHC must typecheck generated code against the actual target
interface before success is reported. Use GHC's existing environment and report
unavailable modules as setup errors; do not add project discovery, automatic
installation, or a custom runtime-location mechanism.

Integration verification therefore needs an externally provisioned target
library. Parsing generated text alone is not sufficient validation. Users write
their own QuickCheck properties; the translator generates representation and
structural demand support, not cost specifications.
