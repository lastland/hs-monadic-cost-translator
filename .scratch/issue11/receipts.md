# Issue 11: caller arithmetic checker slice

Base: `1b95635f315a8b8e38e7d8dcf211192c4319b469`.
Isolated branch: `issue11-arithmetic-checker`.

## Red

Commit `77d48df` adds the public-checker `twice :: Num a => a -> a`
acceptance specimen and direct-observer/nonnumeric negative controls.
The warning-clean Cabal build succeeded. Running only
`cost-translate-test generated-construction` exited 1 at `CallerArithmetic.hs:4`:

```text
unapproved selected method GHC.Internal.Num.+: unknown concrete scalar instance [given Num a_...]
```

The checker emitted that rejection at the binder and retained source use.
The full red log is `issue11-red.log` in the approved OpenCode temporary directory.

## Implementation boundary

`ConstructionArithmetic.callerArithmetic` recognizes only the resolved Prelude
selectors for `+`, `-`, `*`, negation, `/`, `div`, `mod`, `quot`, `rem`,
`fromInteger`, and `fromRational`. It checks the exact wired-in numeric class,
selected predicate, DFun instantiation and context evidence, and each actual
superclass selector/predicate substitution. Given evidence comes from the
checker's existing retained dictionary tracing.

This is the caller arithmetic contract, not a proof that `Num` enforces
strictness. Nonstrict and runtime-observing caller implementations remain outside
the model. The hook runs after protected-operand and demand-data checks. It
does not grant `Eq`, `Ord`, `Show`, other numeric methods, runtime observers,
or arbitrary helpers permission. Complete home-module traversal is unchanged.

Retention controls that formerly used generic numeric literals as invalid
evidence now use unrestricted `Eq` methods. Their source locations and roles
remain checked: boxed actions, erased implicit parameters, and unselected
pattern components. `GenericDictionary.genericScalar` is explicitly clean;
its generic carrier, representation, and nonnumeric method controls still fail.

## Green evidence and limits

The warning-clean GHC 9.10.3 Cabal build and the selected
`generated-construction` group passed after the initial change. The group checks
the actual generated `twiceM` through the public checker and verifies its
exported context remains exactly `Num a`.

The complete generated graph retained 15 unrelated support diagnostics on this
base (FoldableT/default calls, demand usage, shrinker calls and `tval`).
No support body was removed or excused. This branch does **not** claim the full
generated graph is accepted before the independent issues 8/10 integration.
Existing pre-caller lifecycle controls remain enabled.

Additional specimens cover every approved arithmetic operation, integer and
rational literals, standard and home superclass evidence, shadowed Num/(+),
protected thunk arithmetic, unrestricted Eq/Ord/Show, unsupported `abs`, and a
home-imported numeric instance body containing `tval`.

## Separate semantic replay

`semantic/Reference.hs` is an independent handwritten account: function entry
costs one tick; the argument producer costs one tick; sharing evaluates the
producer once. `semantic/Main.hs` compares the complete generated module and
that reference with known expected value `2*n` and cost `2`, for all
`n` from -100 through 100, at both `Int` and a custom strict `Num`.
The instance methods contain no runtime observers or accounting access.

Both `-O0` and `-O2` passed (201 inputs each, both instantiations and reference).
This replay compiled the **full** published module; it did not remove support
declarations. It is separate semantic evidence, not a bypass of the normal
fixture lifecycle's construction gate.

To recreate the replay from this checkout, build the translator with Cabal
without exporting `GHC_PACKAGE_PATH`. Then load the explicitly granted runtime
environment, copy `test/generated-construction/CallerArithmeticSource.hs` to
`semantic/Example.hs`, translate it to `semantic/Generated.hs`, and compile
`semantic/Main.hs` with `-i` pointing at `semantic`, `-package MemoristANT`,
`-package QuickCheck`, and each optimization setting. Keep separate output
directories for the optimization settings.

No external runtime source, fingerprints, README, GitHub surface, source9ab
probe, or ReferencePolicy code was changed. No unfiltered reference suite ran.

## Validation receipts

- `HLINT=/Users/liyao/.local/bin/hlint GHC=/Users/liyao/.stack/programs/aarch64-osx/ghc-9.10.3/bin/ghc sh scripts/validate.sh`
  exited 0. Source policy, ShellCheck, HLint 3.10, policy regressions,
  warning-clean Cabal builds, and the configured runtime-free test selectors
  passed. Log: `issue11-validate.log` in the approved OpenCode temporary directory.
- The final targeted warning-clean Cabal build exited 0; log
  `issue11-build-final.log`. Cabal never inherited an exported `GHC_PACKAGE_PATH`.
- Runtime execution alone loaded the explicitly granted `anf-public-9311279.DdeSzG/toolchain`
  environment. No runtime library was rebuilt or repinned.
- Final `cost-translate-test generated-construction` exited 0, including the new
  shadowed-selector control, updated retention controls, generated Num signature,
  and all existing pre-caller lifecycle specimens. Log: `issue11-final.log`.
  The complete output and its support-only reports remain in the approved
  temporary directory under `caller-arithmetic-artifact--bf0ced49557e663c`.
- Final HLint 3.10 checked all 120 maintained Haskell files and exited 0;
  log `issue11-hlint.log`. `git diff --check` also passed.
