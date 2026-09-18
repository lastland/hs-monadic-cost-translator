# Follow-up: conversion across abstract source types

Status: needs-triage

## Approved current boundary — implemented

The user approved requiring source datatypes and their constructors to be
exported for generated conversions. Private constructors now cause an explicit
source-located unsupported diagnostic before candidate emission/typechecking.
The implementation does not rewrite the source export list.

The CLI acceptance tests cover an abstract exported type, a partially exported
constructor set, and a wholly private datatype. Each must fail at the declaration
without publishing output or recording a generated-code type error. The positive
`Box(..)` example translates, typechecks and returns value 5 at cost 3.

```sh
stack --stack-yaml integration/stack.yaml test cost-translate \
  --test-arguments='diagnostics explicit-constructor-exports'
```

## Future work

Investigate conversion support for abstract source types without requiring
constructor exports. A separate generated module cannot directly match or build
private constructors of the original nominal type. Any future solution must
specify its conversion boundary and preserve source abstraction deliberately;
arbitrary exported accessors must not be assumed to form a lossless conversion.

The motivating source was:

```haskell
module Example (Box, make, readBox, observed) where
data Box = Box Int
make x = Box x
readBox (Box x) = x
observed x = readBox (make x)
```

Its generated `Repr Source.Box` instance previously failed GHC typechecking at
`reprM (Source.Box x)`. Under the approved boundary, it is now an expected
unsupported-source diagnostic. This follow-up is not a blocker for the current
exported-constructor subset and does not authorize source rewrites or unsafe
coercions.
