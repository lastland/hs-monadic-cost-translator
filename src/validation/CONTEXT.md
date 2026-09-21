# Validation

The domain enforcing semantic constraints, inspecting desugared Core for scalar comparisons, restricting comparison operands via the `Scalar` class, and recovering precise type evidence.

## Language

**Semantic Gate**:
A pre-generation check that inspects source declarations, exports, or Core structures and aborts translation with a source-located diagnostic when encountering an unsupported construct.
_Avoid_: Syntax validator, safety filter

**Scalar Comparison Gate**:
The validation pass walking desugared Core (`validateComparisons`) that requires every instantiated `Eq` and `Ord` constraint to operate strictly on scalar types.
_Avoid_: Equality check, Core validator

**Scalar Type**:
A primitive type without internal thunk metadata (`Int`, `Integer`, `Bool`, `Float`, `Double`, `Char`, `()`), safe for direct value-level comparisons.
_Avoid_: Primitive type, unboxed type

**Scalar Class**:
A generated typeclass (`Scalar`) and its closed instance set that restricts polymorphic comparison operands to verified scalar types in generated code.
_Avoid_: Primitive class, comparison witness

**Type Evidence Recovery**:
The targeted desugaring of selected source expressions (`HsVar` and `ExprWithTySig`) to obtain exact instantiated GHC types without using desugared Core for AST emission.
_Avoid_: Type reconstruction, inference extraction
