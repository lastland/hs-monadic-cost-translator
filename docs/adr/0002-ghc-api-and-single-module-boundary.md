# Use GHC for a single-module source translator

Use GHC's API to parse, resolve names, and infer types, with the compiler pinned
through Stack LTS. Reimplementing these services would expand the proof of
concept substantially and make local binding resolution and overloaded primitive
identification unreliable; accepting GHC API version coupling is the trade-off.

Translate one entire selected module before optimization, with explicitly
supported imports and operations. Do not discover projects, traverse user-module
dependencies, or select a function dependency closure. Unsupported definitions
fail the whole module with source-located limitation messages. Match handwritten
replacements by resolved imported identity, not the spelling of a name.

The exact LTS snapshot and GHC pass boundary remain to be validated. Multi-module
support is deferred follow-up work, not an implicit capability of using GHC.
