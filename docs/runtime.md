# External Memorist runtime

Git pins [`hs-pbt/`](../hs-pbt) to commit `3f69b244bedab34c27bad427ddea907d3417c5ae`
on the `fix/private-thunk-allocator` branch of `git@github.com:lastland/memorist-hs-pbt.git`.
This branch is an interim fork of
[`hzelkatz/memorist-hs-pbt`](https://github.com/hzelkatz/memorist-hs-pbt) while
the private thunk-allocator changes await upstream merge. The submodule gitlink
pins the exact commit for reproducible builds, while `.gitmodules` records the
tracked fork URL and branch. The translator package does not depend on
`MemoristANT` at build time.

## Setup and build

From the translator repository root, sync the `hs-pbt/` submodule URL from
`.gitmodules` (so existing clones pick up the fork remote), initialize the
submodule, and build the runtime library:

```sh
git submodule sync -- hs-pbt
git submodule update --init -- hs-pbt
stack build MemoristANT:lib
```

If your environment does not have GitHub SSH keys configured, run
`git submodule sync -- hs-pbt` and rewrite the configured fork URL for that
single `submodule update` invocation without changing global Git configuration:

```sh
git -c url."https://github.com/".insteadOf=git@github.com: submodule update --init -- hs-pbt
```

Pinned revision `3f69b244bedab34c27bad427ddea907d3417c5ae` includes the
`scanl` laziness fix and keeps the thunk allocator private in
`src/Memorist/Base/{Core,ANT,Strict}.hs`. All ten audited runtime modules match
the source fingerprints in
[`test/ConstructionRuntime.hs`](../test/ConstructionRuntime.hs).

Rebuild `MemoristANT:lib` after editing or updating the external runtime. A test
invocation targeting only `cost-translate` can otherwise use the previously
registered runtime, because the translator deliberately has no runtime build
dependency.

## Interface

Neither public `MemoristT` implementation provides `MonadState`. The `Memorist`
class no longer grants it through a superclass. Its explicit `m -> s` dependency
preserves the state-type inference that the old superclass supplied to observers
such as `eval1`.

Both wrappers are abstract. `runMT` remains a read-only observation function,
not a record field that clients can update. Constructors, `genName`, and ANT's
`enclose` are private. Native `lazy` keeps the same allocation and enclosure
rules; only the runtime can lift its underlying state operations. Ordinary
`State` computations, the runtime primitives, and the underlying runtime state
machinery are unchanged.

This restriction removes the safe construction routes used to rewind a live
allocator. It is not a security sandbox: observers may still run separate
computations, and public thunk constructors can still fabricate thunk metadata.
No global-name scheme, scoped-allocation operation, cache, or evaluator is added.

## Check the contract

```sh
sh scripts/stack-test.sh cost-translate:test:cost-translate-test --test-arguments="runtime-allocator generated-construction" --ghc-options='-Wall -Werror'
hlint hs-pbt/src/Memorist/Base/Core.hs hs-pbt/src/Memorist/Base/ANT.hs hs-pbt/src/Memorist/Base/Strict.hs
```

The client-compilation checks expose `mtl` explicitly. Rejection must come from
the absent capability or private representation, not from a hidden package.
The controls execute ordinary `State` and native thunk computations at O0/O2.
The three modified runtime modules (`Core.hs`, `ANT.hs`, and `Strict.hs`) are
warning/lint clean. Other external example modules retain their existing
unused-import, type-default, and incomplete-pattern warnings; the runtime fork
does not change or suppress them. Both `stack.yaml` and `integration/stack.yaml`
set `apply-ghc-options: targets` so `--ghc-options='-Wall -Werror'` applies only
to the selected test targets while leaving external `hs-pbt/` warnings visible.
Generated-output warning checks retain their separately documented scope.
