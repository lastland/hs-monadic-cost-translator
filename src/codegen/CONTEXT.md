# Codegen

The domain constructing parsed abstract syntax tree nodes directly, managing operator precedence structurally, rendering sectioned Haskell code, and maintaining layout invariants.

## Language

**Structural AST Construction**:
The direct creation of `GhcPs` syntax nodes through dedicated combinators (`Cost.Translate.Output`) without parsing source string fragments or invoking lexers.
_Avoid_: String templating, code emission, quotation

**Operator Precedence Management**:
The automatic parenthesization of subexpressions, types, and patterns during AST construction using GHC's internal precedence combinators (`parenthesizeHsExpr`).
_Avoid_: Bracket insertion, precedence fixing

**Pretty-Printing Pass**:
The single-pass rendering of an assembled `HsModule GhcPs` into target Haskell source text via `Cost.Translate.Pretty.prettyModule`.
_Avoid_: Code formatting, printing pipeline

**Section Banner**:
A standardized comment header inserted around declaration blocks in the rendered output to separate common runtime support from module-specific translations.
_Avoid_: Divider, section header

**Extension Tier**:
A graduated set of language extension pragmas (`basicExtensions`, `polymorphicExtensions`, `reflectionExtensions`, `requiredExtensions`) enabled selectively according to module features.
_Avoid_: Extension list, pragma set

**Layout Invariants**:
Formatting rules ensuring that type signatures remain attached to their corresponding bindings, imports are partitioned and sorted, and blank lines separate logical blocks.
_Avoid_: Formatting conventions, indentation rules
