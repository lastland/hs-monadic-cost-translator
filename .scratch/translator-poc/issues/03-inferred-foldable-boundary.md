# Resolve inferred Foldable polymorphism versus list-only replacements

Status: ready-for-human

## Resolution implemented; awaiting parent review

The user chose a direct translated Foldable interface. `FoldableT` now provides
`foldrT`, `foldlT`, and `concatMapT`. Lists delegate to the handwritten runtime
methods. Maybe/pair folds use direct equations, one method-entry tick, no list
conversion, and no callback forcing in the empty case. Source classes are not
otherwise generalized or translated.

`generated-properties` now passes 100 generated-code QuickCheck trials plus
finite infinite-stream observations. Direct Maybe/pair tests check
noncommutative argument order, unused fields/callbacks, and independently counted
entry ticks. The earlier failure description below records the motivating case;
it is no longer the active blocker.

## Comments

- Addressed by commit `91e2d08`: the direct `FoldableT` interface was
  implemented, reviewed, and verified in the full integration suite.
  Status left for the maintainer to close.

## Original failing case

```haskell
module Example where
add x y = x + y
folded xs = foldr add 0 xs
```

GHC infers the valid rank-1 type:

```haskell
folded :: (Foldable t, Num a) => t a -> a
```

The renamed `foldr` resolves to `GHC.Internal.Data.Foldable.foldr`, not a
list-specialized source name. The external replacement is:

```haskell
foldrM :: Th (Th a -> Th b -> MemoristM b)
       -> Th b -> Th (ListT a) -> MemoristM b
```

This list-only interface cannot directly retain the source container parameter.
Source uses at both lists and Maybe are legitimate; a list-specialized target
would discard that source polymorphism.

## PRD interaction

The PRD asks for inferred types without annotations solely for translation,
ordinary rank-1 polymorphism, and reuse of existing handwritten combinators.
The source example needs no signature, but the required replacement fixes its
container to a list. The earlier translator rejected `Foldable t`; the direct
interface now retains that parameter instead of specializing it.

The same question applies to unconstrained wrappers around modern Prelude
`foldl` and `concatMap`. Calls whose source syntax already fixes the container
to a list pass the generated-code tests without annotations.

## Passing regression

```sh
stack --stack-yaml integration/stack.yaml test cost-translate \
  --test-arguments=generated-properties
```

`test/Main.hs:checkGeneratedProperties` retains the original end-to-end case and
now executes its generated value/cost driver successfully. No list signature or
silent type specialization was inserted to make it pass.

## Decision recorded

The user approved direct foldrT/foldlT methods: lists retain handwritten
instrumentation, while Maybe and pairs use direct equations and entry ticks.
Final independent review remains with the parent.
