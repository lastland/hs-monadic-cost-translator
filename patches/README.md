# External Memorist runtime patch

`memorist-private-allocator.patch` keeps the thunk allocator private to the
external `MemoristANT` runtime. The translator does not bundle that runtime or
depend on it at build time.

## Apply and build

Place a compatible `MemoristANT-0.1.0.0` checkout at `hs-pbt/`. From the
translator repository root, check and apply the patch once:

```sh
git apply --check --directory=hs-pbt patches/memorist-private-allocator.patch
git apply --directory=hs-pbt patches/memorist-private-allocator.patch
stack build MemoristANT:lib
```

The patch targets `src/Memorist/Base/{Core,ANT,Strict}.hs`. If the check fails,
inspect the checkout before proceeding; do not force an incompatible patch.
For an already patched checkout, verify it with:

```sh
git apply --reverse --check --directory=hs-pbt patches/memorist-private-allocator.patch
```

Rebuild `MemoristANT:lib` after editing the external runtime. A test invocation
targeting only `cost-translate` can otherwise use the previously registered
runtime, because the translator deliberately has no runtime build dependency.

## Interface

Neither public `MemoristT` implementation provides `MonadState`. The `Memorist`
class no longer grants it through a superclass. Its explicit `m -> s` dependency
preserves the state-type inference that the old superclass supplied to observers
such as `eval1`.

Both wrappers are abstract. `runMT` remains a read-only observation function,
not a record field that clients can update. Constructors, `genName`, and ANT's
`enclose` are private. Native `lazy` keeps the same allocation and enclosure
rules; only the runtime can lift its underlying state operations. Ordinary
`State` computations and the underlying runtime state machinery are unchanged.

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
The controls execute ordinary State and native thunk computations at O0/O2.
The three patched runtime modules are warning/lint clean. Other external example
modules retain their existing unused-import, type-default, and incomplete-pattern
warnings; this patch does not change or suppress them. Generated-output warning
checks retain their separately documented scope.
