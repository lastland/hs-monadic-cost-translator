# Datatypes

The domain covering algebraic datatype representations, demand trees, kind-indexed mappings across higher kinds, dictionary witness classes, budget-bounded demand generation, and direct container folding.

## Language

**Representation Type**:
A generated algebraic datatype (distinguished by a `T` suffix) whose constructor fields are wrapped in explicit thunks (`M.Th`) over the field representation types.
_Avoid_: Wrapped type, thunked type, monadic type

**Demand Type**:
A generated algebraic datatype (distinguished by a `D` suffix) whose constructor fields are wrapped in demand thunks (`M.ThD`) to specify structural evaluation depth.
_Avoid_: Pattern type, mask type

**Kind-Indexed Family**:
A closed type family (`RepOf` or `DemOf`) indexed by an explicit kind parameter that maps types and type constructors to their representation or demand counterparts.
_Avoid_: Type function, kind mapping

**Dictionary Witness**:
A generated typeclass (`ReprApp` or `DemandApp`) carrying equality constraints between type applications and their representation forms to resolve quantified constraints in recursive datatypes.
_Avoid_: Evidence class, proof dictionary

**Quantified Constraint Lifting**:
The technique of packaging runtime class obligations across type applications through quantified constraints and constraint synonyms (`ReprK`, `DemandK`, `GeneratedK`, `ShapeK`).
_Avoid_: Higher-order constraints, class wrappers

**Budget-Bounded Demand Generation**:
A QuickCheck generator algorithm (`gen*Demand`) that decrements a finite integer budget across fields and emits `Skip` upon exhaustion to prevent infinite demand trees.
_Avoid_: Sized generation, fuel generation

**Compatible Shrinking**:
A demand shrinker (`shrink*Demand`) that preserves the observed constructor and either replaces demanded fields with `Skip` or recursively shrinks existing sub-demands.
_Avoid_: Standard shrinking, structural reduction

**FoldableT**:
A generated typeclass providing direct, one-tick container folding (`foldrT`, `foldlT`, `concatMapT`) over thunked data structures without converting containers to intermediate lists.
_Avoid_: Monadic Foldable, list adapter
