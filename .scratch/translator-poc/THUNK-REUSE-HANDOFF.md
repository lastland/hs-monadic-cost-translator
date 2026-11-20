# Handoff: complete thunk reuse through pattern matching

Status: ready-for-agent

Implement the reference-versus-computation rule consistently across argument,
field, case-scrutinee, and pattern-guard translation. Preserve source demand,
sharing, type evidence, and function-entry ticks.

## Starting point

- Reviewed commit: `b5f21ad3e31335cb13192ce3d5f9d800d7290d53`,
  **Reuse existing argument and field thunks**.
- Research and a read-only code audit are complete. The follow-up refactoring
  below has **not** been implemented, compiled, or executed.
- Read `AGENTS.md`, `CONTEXT.md`, `.scratch/translator-poc/CONVENTIONS.md`, and the
  semantics/testing sections of `.scratch/translator-poc/PRD.md` before editing.
  Paths below are repository-relative; line numbers refer to the reviewed commit.
- Inspect the current Git state before starting. Existing untracked generated
  examples and logs under `examples/` are outside this task; preserve them.

## Translation rule and scope

The user's concern is that ANF should not introduce `let y = x` merely to name an
argument that is already a variable. In this target, distinguish an existing
**thunk reference** from a computation that still needs suspension:

| Context | Translation |
| --- | --- |
| Existing thunk needed as an argument, field, or match subject | Reuse its reference, with no new thunk and no force solely for preparation. |
| Computed expression needed as a thunk | Allocate a fresh thunk suspending its computation. |
| Variable needed as a demanded value | Force its existing thunk. |
| Variable/wildcard pattern | Bind the reference or ignore it without forcing. |
| Constructor/literal pattern | Force as required to inspect the value, preserving source order. |

The old argument path emitted `M.lazy (M.force reference)`. This was already
lazy, but added an unnecessary thunk and changed its identity. The reviewed
commit's `emitThunk` supplies the correct normalization rule for arguments,
fields, and callable captures. Complete that rule through matching.

Reuse resolved `Thunk` bindings, not every syntactic `HsVar`: named functions and
other bindings have different target representations. A thunk reference may be
an annotated expression or a dictionary-sensitive factory application, not just
a generated identifier.

Keep the existing policies for explicit source value bindings, their anchors,
generalization, and `sharedAt`/`sharedAtBy`. Avoiding an ANF-introduced alias is
separate from eliminating source-written bindings. Likewise, compound arguments
such as `f (if c then x else y)` need suspension so that an ignored argument does
not demand `c`. A nested source let is not automatically an administrative alias.
Composition intermediates still represent real computations and retain their
existing once-per-capture sharing and ticks.

This task does not require a new ANF intermediate language, common-subexpression
elimination, a runtime change, or a different cost policy. If implementation
evidence requires a broader design change, bring that evidence to the user.

## Implementation sequence

### 1. Establish failing end-to-end regressions

Extend `test/Main.hs` using its existing translate/typecheck/execute helpers.
Start with these supported source shapes:

```haskell
caseAlias x = case x of y -> (x,y)
guardAlias x | y <- x = (x,y)
```

Supply one runtime thunk and verify that both returned fields retain its
`M.tname`, with no extra forwarding allocation. An unused failing input must
remain suspended. Assert source/generated values and entry ticks where demanded;
value and tick checks alone cannot detect zero-cost forwarding wrappers.

**Completion:** the new identity/allocation checks fail against the starting
implementation for the expected wrapper, not a source/type/environment error.

### 2. Generalize the existing thunk-preparation path

Relevant definitions in `src/Cost/Translate/Frontend.hs`:

- `Binding = Thunk O.Expr | ...`: line 302.
- `emitBind` and `emitMatches`: lines 372–396.
- Pattern-guard `BindStmt`: lines 413–417.
- `matchPatterns` and `matchPatternsAt`: lines 443–513.
- Source shared bindings and factory references: lines 538–591.
- `emitExpr` variable forcing: lines 612–628.
- `HsCase`: lines 647–651.
- `emitThunk`, annotations, and suspension helpers: lines 695–721.

Both `HsCase` and pattern-guard `BindStmt` currently call `emitExpr`, then wrap
the result with `M.lazy`. Route these paths through `emitThunk` instead, using
their existing planned `caseThunk`/`guardThunk` names only when allocation is
needed. Sequence the returned preparation statements, then match its reference.

The matching interface currently accepts identifier strings. The proposed
minimal refactoring is:

1. Change `emitMatches` subjects from `[String]` to `[O.Expr]`, and the subject
   components of `matchPatterns`/`matchPatternsAt` to `O.Expr`. Keep pattern-path
   keys as strings: these identify name-planning locations, not thunk references.
2. Store incoming references directly in `Thunk` for variable/as-patterns. Pass
   them directly to `M.force` for demanding patterns instead of rebuilding them
   with `O.v`.
3. Keep actual parameter and constructor-field binder names as strings. Convert
   them with `O.v` when passing their references into matching: function entry,
   positional constructor recursion, and record-field recursion all need this.
4. Keep record slot-name metadata separate from expression references. The
   `ordered`/`used` bookkeeping at lines 481–495 still needs names to construct
   patterns and select used fields; convert subjects at the recursive call.
5. Preserve inline factory applications and annotations intact. Reuse
   `annotateExpression` and the existing type-evidence policy; preserve supported
   scoped annotations and defaulted instantiations.

Keep demanding expression translation in `emitExpr`. Keep `Callable.hs`'s
existing `renderThunk` integration and real composition-intermediate suspension.
Preserve structural AST construction through `Output`, name planning, match
failure continuations, and source pattern/guard ordering.

**Completion:** the refactoring compiles and the new end-to-end checks pass.
Treat this outline as a proposal to validate, not previously compiled evidence.

### 3. Validate the full contract

Cover the following cases through the generated-code/runtime interface:

- Case and pattern-guard aliases, including parentheses, supported type
  annotations, and as-patterns, retain existing thunk identity.
- Computed subjects still receive fresh suspensions. Repeated demand preserves
  their sharing; ignored failing computations remain unevaluated.
- Variable/wildcard patterns remain non-demanding. Constructor/literal patterns
  still demand their subjects when reached. Nested and record patterns preserve
  their existing source-order and fallback behavior.
- Local shared values and polymorphic shared factories survive matching with
  their type evidence. Same dictionary specializations retain identity;
  different dictionary specializations remain distinct. Exercise concrete,
  scoped, and defaulted uses as applicable.
- Existing argument/field/capture regressions and composition-intermediate
  sharing remain green. Source-function entry ticks stay unchanged.

Use the pinned Stack environment, not an unrelated system GHC. At handoff
creation the system GHC was 9.14.1; this project pins 9.10.3. To run these
checks today, initialize and patch the [`hs-pbt/` submodule](../../patches/README.md).

Run the new focused cases and existing thunk groups first, then:

```sh
stack build
sh scripts/stack-test.sh cost-translate:test:cost-translate-test
sh scripts/stack-test.sh --stack-yaml integration/stack.yaml cost-translate-integration
hlint src app test integration/probes integration/test-integration
git diff --check
```

Report unavailable tooling or runtime prerequisites as blockers rather than
passes. Read `README.md` for existing test selection and setup conventions.

**Completion:** record the red/green evidence and actual validation results,
update directly affected documentation if needed, review the diff, and commit
only this task's changes. Use a new branch if the work spans several commits.

## Research basis

These sources establish the translation shape, not a proof that the proposed
Haskell implementation preserves every resource metric:

1. [Launchbury, *A Natural Semantics for Lazy Evaluation* (POPL 1993)](https://launchbury.blog/wp-content/uploads/2019/01/semantics-of-lazy-evaluation.pdf),
   §3.1: variable arguments pass through unchanged; non-variable arguments receive
   fresh lazy let bindings. §3.2/Figure 1 separates binding, application, and
   demanded variable evaluation. §6.1 applies variable operands to constructors.
2. [Flanagan, Sabry, Duba, and Felleisen, *The Essence of Compiling with Continuations* (PLDI 1993)](https://users.soe.ucsc.edu/~cormac/papers/pldi93.pdf),
   Appendix A/Figure 9: `normalize-name` passes values directly and binds only
   non-values. Figure 6 permits alias lets. This is call-by-value ANF; its strict
   sequencing is not the lazy translation to copy.
3. [Li, Xia, and Weirich, *Reasoning about the Garden of Forking Paths* (ICFP 2021)](https://arxiv.org/pdf/2103.07543),
   §4.1/Figures 6–8: expression variables are forced, but argument variables and
   constructor fields pass their thunks directly. Its tick policy is separate
   from this repository's fixed function-entry policy.
4. Local `paper.pdf`, §3.2–3.3, p. 9: the Memorist `takeM`, `repeatM`, and
   `replicateM` examples pass existing thunks directly and suspend computations.
