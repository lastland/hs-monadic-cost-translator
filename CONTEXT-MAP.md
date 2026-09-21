# Context Map

This repo has multiple domain contexts. Each context keeps its own
`CONTEXT.md` describing its ubiquitous language. System-wide decisions live in
`docs/adr/`; context-specific decisions live in `src/<context>/docs/adr/`.

| Context | CONTEXT.md location | Description |
| ------- | ------------------- | ----------- |
| Pipeline | `src/pipeline/CONTEXT.md` | Translation lifecycle, GHC session management, NamePlan pre-allocation, candidate verification, publication gating, and diagnostic reporting. |
| Datatypes | `src/datatypes/CONTEXT.md` | Representation types (`T`), demand types (`D`), kind-indexed families (`RepOf`/`DemOf`), dictionary witness classes, budget-bounded demand generation, and direct container folding (`FoldableT`). |
| Sharing | `src/sharing/CONTEXT.md` | Explicit thunk allocation, call-by-need preservation, unconstrained sharing (`sharedAt`), dictionary-sensitive sharing (`sharedAtBy`), negative delimiters, `Bindings` initialization, and calling conventions (`Callable`). |
| Validation | `src/validation/CONTEXT.md` | Semantic gates, Core-based scalar comparison gate, the `Scalar` class, and type evidence recovery via targeted desugaring. |
| Codegen | `src/codegen/CONTEXT.md` | Direct `GhcPs` AST construction, automatic operator precedence management, single-pass pretty-printing, section banners, extension tiers, and layout invariants. |

## Relationships

- **Pipeline → Validation**: The pipeline invokes semantic gates to reject unsupported source constructs and check scalar comparisons before emitting candidate ASTs.
- **Pipeline → Codegen**: The pipeline coordinates AST emission through codegen builders and invokes the pretty-printer to render candidate source text.
- **Validation → Datatypes & Sharing**: Type evidence recovered during validation informs datatype representations and determines whether value sharing requires type reflection.
- **Datatypes ↔ Sharing**: Representation datatypes wrap fields in explicit thunks managed by sharing combinators; demand generators construct demand trees that mirror thunk structures.
- **Codegen → Datatypes, Sharing, & Validation**: Codegen provides the underlying structural builders (`Cost.Translate.Output`) that construct AST nodes for datatypes, sharing primitives, and validation assertions.
