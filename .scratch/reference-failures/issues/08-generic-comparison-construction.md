# Resolve generic equality construction rejection in captured helpers

Status: needs-triage
Type: task

## Case and observed diagnostic

- Selector: `mutually-recursive-captured-helpers`
- Source: `test/cases/mutually-recursive-captured-helpers/Example.hs`
- Caller: `test/cases/mutually-recursive-captured-helpers/Case.hs`
- Independent reference: `test/cases/mutually-recursive-captured-helpers/Reference.hs`
- Baseline and provenance: [audit map](../map.md).

The complete-output construction guard reports:

```text
unapproved selected method GHC.Classes.==:
unknown concrete scalar instance [given Eq a]
```

The receipt includes generated helper and call sites at generated lines 193,
200 and 214. No ordinary caller build or reference comparison is established
after this guard rejection. Recursion itself is not the reported failure.

## Intended behavior and acceptance

The user approved a bounded inferred-local comparison candidate. Its separate
implementation and review do not establish acceptance of this case, and it is
not integrated into this baseline. Establish the permitted comparison evidence and
closure without treating any arbitrary `Eq a` dictionary as a concrete scalar
instance. Preserve captured environments, recursion, laziness, ordinary caller
interfaces and the unchanged independent reference and cost assertions.

Require the full complete-output guard before ordinary caller compilation and
then unchanged boundary/seeded value/cost comparisons and existing optimized
postchecks. Candidate work or removing one rejection does not establish success.

## Reproduce

Prepare the runtime using `docs/runtime.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=mutually-recursive-captured-helpers
```

## Comments

- Recovery verified at integrated commit
  `9b8492c3f9d354e2411bc3c2a5700aa89fc62aee`: the unchanged source, ordinary
  caller and independent reference pass the complete shared lifecycle, including
  O0/O2 boundary cases and 200 randomized value/cost comparisons. Construction
  controls, direct-function and source-scope checks pass in the same focused run.
  Both pinned integration reviews have zero findings. This establishes recovery
  of this case without granting arbitrary open `Eq` dictionaries permission.
- After split integration, this case passed again through the per-source runner
  with complete guards, ordinary callers and O0/O2 comparisons. Its catalog entry
  is explicitly removed in the promotion checkpoint. The historical `e5d4652`
  failure receipt remains preserved; this case is now a passing regression.
