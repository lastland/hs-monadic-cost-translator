# Demand Eq/Show certificates: child-session receipts

Base: `4571b755cddedbbb5c7502746ecc8bcbdd6cdc29`.
Branch: `demand-eq-show-certificates`.
No runtime, generator, public checker interface, or `src/` changes.

## Scope and limits

- Runtime certificates cover only the pinned stock Eq/Show instances for
  `ThD scalar`, where `scalar` has an actual closed registered standard instance.
  The selected method, DFun identity, instantiated predicate, and field
  dictionary must match. A given dictionary does not qualify.
- Home certificates require a resolved standard Eq/Show stock-deriving request,
  the actual closed DFun, and its actual selected implementation bodies.
  The checker replays retained method dependencies, including defaults.
  Home data must be monomorphic, non-newtype, vanilla, and have only scalar or
  `ThD scalar` fields, including at least one `ThD scalar` field.
  No recursive, polymorphic, nested-home, other demand-container, via,
  anyclass, or handwritten method certificate is added.
  Unknown `GHC.Magic.dataToTag#` dependencies still fail replay; the parent's
  broader publication audit does not expand this certificate scope.
- The pinned compiler's stock generation establishes the definition proof.
  Matching a source deriving request to the actual standard class/type and
  checked instance excludes handwritten equations; duplicate instances cannot
  typecheck. Matching the raw dictionary constructor links that proof to its
  selected fields. Replay checks the method dependencies separately.
- Existing observer/accounting/default rules and genuine Gen/Property boundaries
  remain unchanged (#7 and #9). This does not close #8/#10/#11. Strict complete
  output remains rejected, including at `liftShrink`; this is not a full
  construction-contract completion.

## Public-boundary RED and GREEN

Seam: `checkGeneratedConstruction`, complete source modules, real runtime and
callers. Each specimen typechecks independently before the checker verdict.

- RED: `sh scripts/stack-test.sh --test-arguments=generated-construction`
  failed for `DemandScalarMethods.hs`, including runtime `Eq (ThD Int)`,
  `Show (ThD Int)`, derived `BoxD` methods, and their real callers.
  Log: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.SjVPd2`.
- Final focused GREEN (including generated lifecycle guards):
  same command, PASS.
  Log: `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.qzsh9X`.
- Positive coverage includes `==`, `/=`, `show`, `showsPrec`, and `showList`,
  runtime and home dictionaries, inline and standalone stock derivation.
- Negative controls cover observing custom Eq/Show dependencies, protected
  `ThD (Th Int)`, `Annot`, `USet`, unresolved/given field dictionaries,
  manual methods, shadowed deriving classes, unsafe forged dictionaries, and
  `tval`. Existing observer/accounting and QuickCheck-role controls also pass.
- Wrong-pin experiment: loaded a temporary `ConstructionRuntime` with the
  Demand pin's first word changed to zero, then called the same public checker
  on `DemandScalarMethods.hs`. It rejected with
  `construction setup: audited source fingerprint mismatch`.
  Exact log:
  `/private/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/opencode/demand-wrong-pin-ghci.log`.

## Actual generated diagnostic delta

Fresh complete translation of `test/cases/anf-demand/Example.hs`:
`/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/construction-artifact--8bb02fe7b5dc96c8/Generated.hs`.

The public checker produced **89 baseline reports and 65 new reports**.
After normalizing only GHC-generated type-variable uniques, the multiset delta
is **24 removed, zero added**. Every removal is an Eq/Show obligation for the
actual pinned `ThD Int` instance or actual stock-derived `BoxD`/`RecordD`
instances. All unrelated method, observer, accounting, and shrinker reports
remain. The maintained test translates this fixture afresh, retains diagnostics,
requires observer/accounting and `liftShrink` failures, and forbids residual
pure Eq/Show reports.

Baseline experiment: loaded `GeneratedConstruction` in
`stack ghci cost-translate:test:cost-translate-test --no-load` with an explicit
temporary `ConstructionEvidence.hs` target obtained from `git show BASE:...`.
Called `constructionReports` on the same generated file in baseline and current
sessions. No files from another worktree were copied.

Artifacts under
`/private/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/opencode/`:

| File | SHA-256 |
| --- | --- |
| `demand-baseline-reports.txt` | `87831ea7169cfbe4538826480cf1443a6956c790411b652503941a9b31d71494` |
| `demand-current-reports.txt` | `9044ff656ff767352f152058dbfab6b6cee527a54dcad78d9dfeb8da08710f8b` |
| `demand-diagnostic-delta.txt` | `f38984d84c1c9a0465c520f2f84f91d6fd539a7a66305b7cf5c475ab8a5cc1ee` |

Generated file SHA-256:
`2a478f980e6c9e3c23375b184fe605450a64704a71a90418eac66faf2badef55`.

## Validation

- `sh scripts/check-source.sh`: PASS, 75 source files.
- Default HLint invocation correctly failed because PATH selects HLint 3.5.
  Use the installed pinned tool:
  `HLINT=/Users/liyao/.local/bin/hlint sh scripts/validate.sh`.
- Aggregate validation passed, including the final post-change rerun:
  `/private/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/opencode/demand-final-validate.log`.
- Full unfiltered Stack reference suite intentionally not run in this child.
  The parent owns integration and review dispatch. No GitHub or delegation
  operation was performed.
