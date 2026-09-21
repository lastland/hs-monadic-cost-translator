# Sharing

The domain governing explicit thunk allocation, call-by-need preservation, polymorphic and dictionary-keyed thunk reuse, top-level value initialization, and calling convention validation.

## Language

**Call-by-Need Preservation**:
The property ensuring that lazy value bindings and captured expressions evaluate at most once per execution instance, matching Haskell runtime sharing behavior.
_Avoid_: Lazy caching, memoization

**Thunk Allocation**:
The creation of an explicit named thunk (`M.Th`) wrapping a suspended computation, retaining its identity across multiple references.
_Avoid_: Suspension creation, delayed evaluation

**Unconstrained Sharing (`sharedAt`)**:
The allocation of a single runtime thunk at a fixed reserved name, shared across all type instantiations of an unconstrained polymorphic value.
_Avoid_: Monomorphic sharing, static sharing

**Dictionary-Sensitive Sharing (`sharedAtBy`)**:
The allocation of distinct thunks keyed by structural type representations of the generalized variables appearing in dictionary constraints.
_Avoid_: Constrained sharing, class-keyed caching

**Structural Type Key**:
An integer sequence computed by `sharingTypeKey` from `Reflection.SomeTypeRep` that uniquely encodes qualified type constructors, kind arguments, and applications.
_Avoid_: Type hash, string key

**Negative Delimiters**:
Negative integer markers (`-1` and `-2`) inserted into thunk name ancestry to ensure structural type keys remain prefix-free and isolated from ordinary allocator names.
_Avoid_: Namespace separators, key prefixes

**Bindings Record**:
A generated data structure grouping top-level value thunks and functions, created once per evaluation by `initialize :: MemoristM Bindings` to preserve top-level sharing across calls.
_Avoid_: Global environment, state record

**Callable**:
An expression classified as a named function or composition spine, validated against its arity and calling convention to govern adapter emission.
_Avoid_: Function reference, invokable
