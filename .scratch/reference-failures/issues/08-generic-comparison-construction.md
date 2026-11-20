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

Prepare the runtime using `patches/README.md`, then run:

```sh
sh scripts/stack-test.sh --test-arguments=mutually-recursive-captured-helpers
```
