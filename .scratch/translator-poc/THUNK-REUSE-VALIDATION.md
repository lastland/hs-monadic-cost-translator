# Thunk reuse and ANF validation

Baseline: `b5f21ad3e31335cb13192ce3d5f9d800d7290d53`.

Contract: [THUNK-REUSE-HANDOFF.md](THUNK-REUSE-HANDOFF.md), together with the
PRD's semantics and testing decisions. The original handoff is retained unchanged
as the implementation specification.

## Red/green evidence

The new generated-code regressions were run before changing the translator:

- `thunk-case` translated and compiled successfully, then failed with
  `caseAlias: match alias changed thunk identity`.
- `thunk-guards` translated and compiled successfully, then failed with
  `guardAlias: match alias changed thunk identity`.

After the matching-reference change, both groups passed. Each group checks plain,
parenthesized, scoped-annotated, and as-pattern aliases. The checks observe input
and result thunk names, allocator progression, unused failures, entry ticks, and
repeated-demand sharing. The existing `thunk-forwarding`, `thunk-fields`,
`thunk-captures`, `thunk-sharing`, and `runtime-failures` groups also passed.

The checks use the existing public boundary: translate a source module, typecheck
the generated candidate, compile a driver, and observe execution against the
external runtime. No generated-AST snapshots or runtime changes are required.

## Broader ANF regressions

- `anf-demand`: computed case/guard subjects have one fresh suspension; ignored
  failing conditionals and calls stay suspended; explicit source-written
  `let y = x` retains its anchor. Repeated demand shares computation costs.
  Variable/wildcard patterns stay non-demanding; constructor, literal, nested,
  and demanding as-patterns inspect only reached structure. Case and guard
  record patterns retain source order. Scalar returns, primitives, conditionals,
  and short-circuit Booleans demand their required inputs.
- `thunk-matching-sharing`: local and top-level polymorphic values retain names
  across matching and type instantiations. Dictionary-sensitive values retain
  names at the same specialization and distinguish different dictionaries,
  without keying on unrelated result parameters. Concrete, scoped, locally
  scoped, and defaulted uses typecheck and preserve suspended failures. Demanded
  defaulted case/guard aliases also share one Integer specialization across calls.
- `anf-properties`: each invocation translates and compiles a fixed source
  fixture once. A deterministic boundary check covers empty/multi-output
  captures and repeated source-variable demand. It then runs 100 seeded and 100
  randomized samples, checking every pair-demand mask for each sample. Inputs
  range from -40 to 40, repetition counts from 1 to 8, and list lengths from 0
  to 5. Checks cover values, thunk identities, separate versus shared work,
  computed captures, nested composition intermediates, and exact entry ticks.
  Failure reports include shrunk samples and QuickCheck seed/size information.

The pair cost oracle counts one source entry plus the distinct demanded work
calls. The mapped-pair oracle counts one wrapper, `n+1` map entries, `n` pair
entries, and zero, one, or two shared capture-work entries when demanded.
Returning a shared value through two separate `keep` calls counts both `keep`
entries but the shared `work` entry only once.

## Test sensitivity

Temporary translator mutations were applied one at a time and removed. Each
final mutation compiled and generated a typechecked driver, then failed at
runtime:

| Deliberate defect | Detecting group | Observed failure |
| --- | --- | --- |
| Restore unconditional case forwarding suspension | `thunk-case` | `caseAlias: match alias changed thunk identity` |
| Evaluate computed preparations before suspension | `anf-demand` | `unused input demanded` |
| Copy a demanded thunk's annotation instead of recording its shared usage | `anf-properties` | Both returned shared fields cost 5 ticks instead of 4 |
| Reverse supplied record-subpattern order | `anf-demand` | Previously skipped undefined record field was evaluated |
| Remove source-function entry ticks | `thunk-case` | `caseAlias: match preparation changed entry ticks` |

The annotation-copy mutation initially survived the original property fixture.
That exposed a gap between primitive/runtime forcing and translated scalar
returns. Adding `returnedShared x = let y = work x in (keep y,keep y)` made it
fail for the both-fields demand. Restoring normal forcing made the expanded
property group pass. These checks establish sensitivity to the listed defects,
not an exhaustive mutation score or a correctness proof.

## Scoped-annotation review follow-up

Independent review of `625b2e9` found a further scoped-type regression despite
the passing suite. A case/guard alias can carry `(x :: M.Th a)` into a local
function whose `forall a` shadows the annotation's original type variable.
The source typechecks, but the generated candidate then fails with
`Expected: M.Th a1; Actual: M.Th a`.

The `scoped-case-alias-shadow` and `scoped-guard-alias-shadow` end-to-end
regressions reproduce that error. Temporarily restoring the old case and guard
preparation makes each exact regression pass with value 1 and three ticks; those
control changes were removed. Additional dictionary-factory regressions distinguish Int
and Integer behavior, so discarding the annotation and defaulting its type
cannot silently pass the value checks.

The user approved a scoped-only plain-`let` exception to alias avoidance on
2026-11-15. Both matching boundaries keep the same annotated reference in its
original lexical scope, then store the plain name in pattern aliases. The binding
adds no `M.lazy`, force, tick, or Memorist thunk allocation. Ordinary references,
closed annotations, and computed subjects retain their existing paths. The gate
checks both GHC's free-variable evidence and the emitted type, excluding source
kind variables erased from the target. It also retains the existing policy for
fresh annotation variables. See `README.md` and `CONVENTIONS.md` for the explicit
rule and rationale.

With the retained repair, all six shadowing selectors pass: direct case/guard
aliases return 1 in three ticks; dictionary-factory aliases return 1 in three
ticks at Int and 2 in four ticks at Integer. The ordinary case/guard identity and
allocator checks, matching-sharing checks, deterministic ANF demand tests, and
seeded/randomized ANF properties also pass in the same focused run.

## Interpretation limits

- `tname` equality checks reference identity, not physical host-language
  evaluation counts.
- The next fresh name observes allocation at the current naming level, not a
  global heap-allocation count. Tests combine it with returned thunk identities
  in controlled fixtures.
- Tick expectations count source-function entries under explicit demands, not
  GHC-optimized execution time. Failing computations have no promised cost.
- Infinite and partial values require finite observations and bounded demands;
  displaying or fully comparing them can itself introduce demand.

## Toolchain

`stack exec -- ghc --version` reported GHC 9.10.3. Stack uses LTS 24.22 and
QuickCheck 2.17.1.0, with the existing external `hs-pbt/` checkout. The translator
package gains no runtime or QuickCheck build dependency.

## Validation results

The following commands passed with the correct implementation restored:

```sh
stack build
stack test cost-translate:test:cost-translate-test
stack --stack-yaml integration/stack.yaml test
hlint src app test integration/probes integration/test-integration
git diff --check
```

The integration project passed all 37 P1–P7 tests and the `runtime-scanl`,
`runtime-polymorphic-sharing`, and `runtime-higher-kinded` gates. The root suite
passed its existing generated-code, diagnostics, demand, and cost checks together
with the new ANF groups. Focused runs also verified each new selector, including
the demanded defaulted aliases. HLint reported no hints.

Fixture warnings about deliberately partial calls/redundant patterns and the
linker's redundant `-U` warning did not prevent successful validation. No tooling
or external-runtime prerequisite was unavailable. Existing generated examples
and logs were left untouched and excluded from commits.
