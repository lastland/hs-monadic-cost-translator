# Handwritten feasibility probes (with historical loading findings)

Current status: **all 37 P1–P7 tests pass**. The two P8 bytecode-loading tests and
their TestSupport harness were retired after the actual generated-code
subprocess acceptance suite superseded them. The empirical loading findings
below are historical evidence, not a description of the current harness.

Artifacts:

- `integration/probes/Probes.hs` — P1..P7, handwritten Memorist-style
  translations exactly per `CONVENTIONS.md`, each with an independent cost
  derivation in comments.
- `integration/test-integration/Main.hs` — tasty driver for everything.
- `integration/stack.yaml`, `integration/cost-translate-integration.cabal`.

## How to run

```sh
stack --stack-yaml integration/stack.yaml test cost-translate-integration
```

The root translator now builds independently with `stack build`. The separate
integration project uses the external runtime and QuickCheck 2.17.1.0. See
`IMPLEMENTATION-STATUS.md` for generated-code checks and current delivery status.

## Cost derivation table

`n` = input length (or `k` per probe), ticks counted under `fullDemand`
unless noted. "usage-set" = forcing the same thunk name several times
counts its cost once.

| Probe | Program | Derived cost | Verified |
|---|---|---|---|
| P1 | `mapSucc xs = map succ xs` | entry 1 + mapMM n+1 + succM n = **2n+2**; partial demand k-of-n: **2k+1** | exact + 100 QC |
| P2 | `sumWith n xs = foldr (add3 n) 0 xs` (callback 1-of-3) | entry 1 + foldrM k+1 + add3M k = **2k+2** | exact + 100 QC |
| P2 | `mapAdd n m xs = map (add3 n m) xs` (callback 2-of-3) | entry 1 + mapMM k+1 + add3M k = **2k+2** | exact + 100 QC |
| P3 | `example n xs = map (add n) xs where add x y = x + y + n` | entry 1 + mapMM k+1 + addM k = **2k+2** | exact + 100 QC |
| P3 | same with `n = expensive 3` (local value thunk) | **2k+6** (k>=1; expensive 4 counted once via usage-set); **2** when k=0 (see finding 1) | exact + 100 QC |
| P3 | non-shared contrast (rebuild n per element) | **6k+2** (k>=1; =2 at k=0) | exact + 100 QC |
| P4 | `map (succ.succ)` via `lazy (composeM succM succM)` | entry 1 + mapMM k+1 + 2k succM = **3k+2**; cost-identical to `ML.mapSuccM2` under random demands (`compareOnSame` (==)) | exact + 100 QC |
| P4 | 3-chain `lazy (composeM sM sM >>= composeM sM)` | **4k+2** | exact + 100 QC |
| P5 | `answer x = x + head numbers + head numbers`, `numbers = expensiveList 3`, hoisted once | entry 1 + 2 headM + 1 spine tick once = **4**; non-shared variant **5** | exact |
| P5 | element-expensive `numbers2 = countdown 3 : []` | shared **7** (countdown 4 paid once), non-shared **11** | exact |
| P6 | match-first `takeA n (x:xs) | n <= 0 = []` | on defined input **n+1** (same as takeB and `ML.takeM`) | exact + 100 QC |
| P6 | guard-first `takeB n _ | n <= 0 = []` | **n+1**; `takeB 0 ⊥` = cost **1**, value `[]` | exact |
| P6 | `takeA 0 ⊥` | demanded evaluation **throws** (column forced before guard) | IO try/evaluate |
| P6 | partial demand `ConsD (Need 0) Skip` over `1:⊥` | **1** (suspended tail never forced, bottom untouched) | exact |
| P7 | `myConcat = foldr myAppend []` with translated append | foldrM K+1 + myAppend L+K + entry 1 = **L+2K+2**; cost-identical to `ML.concatM` under random demands | exact + 100 QC |

All value-correctness checks (`correct` parity with the pure source
semantics) pass, including under QuickCheck.

## Empirical answers required by the PRD

### GHC_PACKAGE_PATH and package resolution

- **stack sets `GHC_PACKAGE_PATH` for test suites** to
  `<local install pkgdb>:<snapshot pkgdb>:<ghc global package.conf.d>`
  (print with `stack path --ghc-package-path`).
- **The ghc library picks it up automatically.** With
  `backend = interpreterBackend`, `ghcLink = LinkInMemory`, and only
  `packageFlags = [ExposePackage "-package MemoristANT" (PackageArg
  "MemoristANT") (ModRenaming True [])]`, `setSessionDynFlags`/`load`
  resolves MemoristANT with **no explicit `packageDBFlags`** — no parsing
  of the env var needed under `stack test`.
- The explicit fallback (`packageDBFlags = map (PackageDB . PkgDbPath) dbs`
  from a hand-parsed env var) was also validated on its own
  (`CT_FORCE_EXPLICIT_DBS=1` above): it works. Keep it for running the
  harness outside stack (e.g. plain `cabal test`), where the env var is
  absent: without it `-package MemoristANT` fails with "cannot satisfy
  -package MemoristANT".
- Loading interpreted bytecode that imports package modules works
  out-of-the-box on darwin/arm64 with stack's default build settings —
  no `-dynamic` needed. (MemoristANT is installed as a static `.a` only;
  the RTS linker loads it and binds against the test binary's symbols.)

### Finder: module `Foo.Cost` in file `FooCost.hs`

**Accepted.** With `importPaths = [root]` and file targets for all three
modules, GHC loads `X.Cost` from `XCost.hs` next to `X.hs` and links the
`Driver` that imports `X.Cost`. The subdirectory form `X/Cost.hs` also
works. Per CONVENTIONS the generator may emit the flat `FooCost.hs`
layout; no forced `Foo/Cost.hs`.

### GHC 9.10.3 API notes discovered while building the harness

- DynFlags/flag types live in `GHC.Driver.DynFlags` (re-exported by
  `GHC.Driver.Session`): `DynFlags(..)`, `GhcLink(..)`, `PackageFlag(
  ExposePackage)`, `PackageDBFlag(PackageDB)`, `PkgDbRef(PkgDbPath)`,
  `PackageArg(..)`, `ModRenaming(..)`. `GHC.Unit.Types` does NOT export
  `PackageArg`/`ModRenaming` in 9.10.
- `interpreterBackend :: GHC.Driver.Backend`; setting
  `backend = interpreterBackend` is the bytecode-producing replacement
  for `hscTarget` (no `hscTarget` field manipulation needed).
- `guessTarget :: GhcMonad m => FilePath -> Maybe Phase -> Maybe
  TextEncoding -> m Target` — lives in the Ghc monad, not IO.
- `simpleImportDecl` from `GHC.Hs.ImpExp`; `mkModuleName` from
  `GHC.Unit.Module`; `IIDecl` from `GHC.Runtime.Context`.
- `dynCompileExpr` evaluates fully-qualified driver names fine, but the
  interactive context has **no Prelude** — do not put type annotations
  like `:: (Integer, Bool)` inside the expr; pin types on the host side
  via `fromDynamic`.
- `fromPure1 f` applied to nothing is ambiguous (`Rep a ~ Int` does not
  pin `a`): drivers must apply it (`fromPure1 doubleM (5::Int)`) — same
  for `correct`.

## Convention-validation findings

Nothing in CONVENTIONS failed. Two findings that matter for spec users:

1. **Undemanded thunks cost 0 (P3, empty input).** The local value
   `n = expensive 3` is only paid when some `addM` invocation forces it:
   `[]` input costs 2, not 6. This is correct MemoristANT laziness
   semantics, but it means (a) exact-cost QC properties must branch on
   `null xs` (done in `p3Example3Expected`), and (b) integration drivers
   that want to *observe* an inner computation must demand its result.
2. **Source-order callback values (P3).** For
   `map (add n) xs where add x y = x + y + n`, the mapped value is
   `e + n + n` — the callback's first parameter *is* the passed `n`, and
   the captured `n` is forced again in the body. The translation handles
   this correctly (and both forcings hit the same thunk name, still
   counted once); only the hand-written reference implementations in
   tests must use the right arithmetic.
3. Guard-order (P6) confirms the CONVENTIONS rule that the translator
   must preserve source equation order: forcing the list column before
   the guard makes `take 0 ⊥` fail exactly like the match-first source,
   while the guard-first form returns `[]` at cost 1. Cost-parity on
   defined inputs holds between both variants and `ML.takeM`, so the
   difference is purely semantic (bottom-strictness), as the paper's
   guard-first note anticipates.
4. Composition-chain convention (P4) typechecks and costs as specified
   at both 2- and 3-chain shapes; the 2-chain is cost-identical to the
   paper's `mapSuccM2` under randomly generated demands.
5. Partial applications at non-binary arity (P2) work in both callback
   flavors (`lazy (return (add3M tn))` where a binary callback is
   expected via foldrM, `lazy (return (add3M tn tm))` where unary via
   mapMM) with no adapter ticks, matching the `curryKM` convention.
6. Top-level-value hoisting (P5): per-invocation hoisting + usage-set
   gives "expensive counted once" (4 vs 5, 7 vs 11 in the non-shared
   contrasts). Note the spine-expensive shape only pays the cons actually
   demanded by `head` (1 tick), which is why the element-expensive
   variant is the sharper demonstration.
