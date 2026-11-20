# Wider-kind stock provenance production handoff

## Pins and scope

- Branch: `stock-typed-provenance`
- Immutable base: `e4633c8d5fea09498c9d52d8be18217fe0afa4fb`
- RED public indexed fixture: `3700302`
- GREEN implementation: `e28ca59`
- The following controls/reference commit completes the maintained tests.
- No translator implementation, runtime, public constraints, emitter, independent
  reference, or reference observer changed. No GitHub operation or agent ran.

`ConstructionStockRequests` checks standalone stock requests using the accepted
module's flags, existing session and `TcGblEnv`. It matches complete generic
heads, dependent kinds, ordered contexts, actual Names and a unique DFun.
Via/AnyClass requests never enter elaboration. Attached requests use their
checked TyCon telescope and actual inferred DFun context. Metadata errors fail
closed. Original-source modules receive an empty provenance set.

`ConstructionDemandMethods` retains the base's applied-stock, owning-context and
runtime-delegation backend. Eligibility changes do not waive field proofs,
dictionary checks or recursion/progress requirements.

## Maintained controls

Ten complete positive graphs are registered: `StockHigher`, `StockHigherSum`,
`StockHigherStandalone`, `StockHigherPoly`, `PolyAttached`, `PolyStandalone`,
`PolyRepresentations`, `DependentStandalone`, `PolyExplicitKinds` and
`DefaultStockStrategy`. The additional minimal RED fixture is `StockIndexed`.
All are separately typechecked before the public guard runs.

The original 51 negative definition sites are registered across
`StockHigherEscapes` (19), `StockHigherSpecialized` (4), `StockHigherVia` (1),
`PolyEscapes` (19) and `PolyHeads` (8). Additional provenance controls cover
context specialization, AnyClass/default methods, and all three ViaSelf caller
reports.

Whole TH graphs still produce two public TH syntax-gate reports. A supplemental
catalogue check calls the production helper and verifies that neither actual
manual generic Eq DFun registers, while the legitimate generic Show does.
Its runtime lookup precedes TH loading, matching the public guard; reversing
that order reproduces GHC 9.10.3's `No mi_insts in PIT` panic.

Wildcard requests typecheck separately, then fail closed with identical
metadata diagnostics on repeated checks. Three duplicate-head compiler probes
reject stock/manual, TH-manual, and context/overlap variations.

The 30 older controls were replayed against an executable built from an exact
base archive and the production executable. Report multisets retain sites,
reasons, dictionary identities, types and counts after normalization of GHC's
fresh printed uniques. Only the expected TagParameterized change occurs:
two old Eq/default reports become acceptance. StockGrowth remains exactly 32
reports, now also asserted by the maintained suite.

## Validation receipts

All commands use GHC 9.10.3. Stack uses the private
`.scratch/stock-provenance/stack.yaml`, with explicit `MemoristANT:lib` and no
ambient `GHC_PACKAGE_PATH`. Its unchanged external runtime matches all ten
audited source hashes, including Core
`9ad5430a569dbdbb00a35a0d48e463e2`; `9ad` is a fingerprint, not a Git revision.

- `scripts/validate.sh`: PASS, including source/shell/HLint checks, policy tests,
  Cabal build and its configured test selection.
- HLint 3.10 after the final test assertion: PASS, 141 Haskell files.
- Expanded focused Stack suite: PASS, log
  `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.2tjw5m`.
  Includes generated-construction, source-scope, higher-kinded-stock-context,
  runtime-demand-bundle, and all four representative kind-related groups.
- Fresh ordinary ReferenceRunner publications and unchanged independent
  reference comparisons pass for 080/082/111/112, including O0 and O2.
  Representative O0/O2 runs are now registered in `test/Main.hs`.
- Frozen complete 080/082/111/112 artifacts all ACCEPT through the public guard
  under the production backend, without origin exemptions. Receipt:
  `.scratch/stock-provenance/frozen-replay.log`.
- Thirty-control differential receipt:
  `.scratch/stock-provenance/{base-guard-probe,guard-probe}-controls.log`;
  replay script: `.scratch/stock-provenance/compare-controls.py`.

The **unfiltered full Stack suite does not pass**. It stops at
`anf-scoped-renaming`: repeated-factory values agree, but generated/reference
costs are `24/17`, `32/18`, `56/35`. The exact base archive reproduces the same
boundary sample and all three differences. Production log:
`/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.ceuiYx`;
base log:
`/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.qQl8Le`.
No oracle, expected cost, emitter or runtime was changed to hide this failure.
Groups after the fail-fast stop are not claimed as fully audited; the parent
retains the separate 171-case audit.

The local Stack YAML, probes, build outputs and differential logs are
untracked provisioning/receipts, not maintained project files.

## Review follow-up: shared pure head-shape validation

The verified low-severity duplication finding on `e577265` is addressed by
`stockHead` in the existing metadata module `ConstructionStockRequests`.
It owns the ordinary lifted vanilla datatype, complete distinct exhaustive
variable head, and dependent-kind checks.

`genericHead` calls it with the variables and predicate from its actual typed
request/DFun. `stockMethods` independently calls it with the actual Core
dictionary binders and `exprType body`. The separate dictionary `exprType`
equality, predicate-slot checks, worker identity, ordered field checks, method
binders/slots and owning-method obligations remain unchanged. The helper
returns only shape metadata, not provenance or executable permission.

No README, reference, runtime, helper permission, or source-policy change is
part of this follow-up. The 30-control differential replay again passes,
including the same sole TagParameterized acceptance change relative to the
immutable base and exactly 32 StockGrowth reports.

Follow-up validation:

- `scripts/validate.sh`: PASS (including HLint, 141 files, and the configured
  Cabal test selection).
- Expanded focused Stack selection: PASS,
  `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.dXJop3`.
  This reruns the full construction controls (all 51 negative sites, ViaSelf's
  three caller reports, StockGrowth's 32 reports), source scope, stock context,
  runtime demand bundle, and the four representative kind-related groups with
  their unchanged independent references at O0/O2.
- Unfiltered full Stack reference suite: RED at the known strict
  `anf-scoped-renaming` boundary, with the same `24/17`, `32/18`, `56/35`
  generated/reference cost differences documented above; no expectation was
  weakened. Receipt:
  `/var/folders/3c/qfxwcbpj7q590cwh0_scnyl80000gn/T/stack-test.KnnTeP`.
  Later groups remain outside this fail-fast full-run result.
