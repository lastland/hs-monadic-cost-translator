# Resolve constrained-sharing reference cost differences

Status: needs-triage
Type: task

## Cases and evidence

These seven cases fail value/cost comparison at the [audit pin](../map.md).
Complete-output guards and ordinary caller compilation succeed first. Each
source is named exactly in the map; callers and unchanged independent references
are `test/cases/<selector>/Case.hs` and `test/cases/<selector>/Reference.hs`.
In particular, `constrained-sharing-same-dictionary` uses the source from
`constrained-sharing-different-dictionaries`, not a same-named source directory.

At the common boundary sample recorded in the map:

| Selector | Probe | Value | Generated cost | Reference cost |
|---|---|---:|---:|---:|
| `constrained-sharing-all-dictionaries` | `interleaved-all-dictionary-keys` | 100 | 16 | 10 |
| `constrained-sharing-constructor-dictionaries` | `interleaved-constructor-dictionaries` | 68 | 16 | 10 |
| `constrained-sharing-different-dictionaries` | `interleaved-dictionaries` | 32 | 20 | 11 |
| `constrained-sharing-generic-caller` | `interleaved-dictionaries` | 32 | 28 | 19 |
| `constrained-sharing-nested-allocations` | `interleaved-dictionaries` | 32 | 28 | 13 |
| `constrained-sharing-same-dictionary` | `repeated-same-dictionary-pairs` | 28 | 16 | 9 |
| `constrained-sharing-unconstrained-parameter` | `interleaved-unconstrained-element-types` | 28 | 16 | 9 |

All reported probes return matching values. The first two cases also report
repeated dictionary costs 8 versus 5. Different-dictionaries reports Int
8 versus 5 and Integer 12 versus 6; generic-caller reports 12 versus 9 and
16 versus 10; nested-allocations reports 12 versus 6 and 16 versus 7.
Same-dictionary and unconstrained-parameter each also report fixed costs
4 versus 3 and repeated costs 8 versus 5.

This groups the constrained-sharing fixtures and their repeated-specialization
cost symptoms, not a proof that every discrepancy has one source-global cause.
Nested allocations and generic callers need their own accounting.

## Intended behavior and acceptance

Match each unchanged independent reference, preserving dictionary separation,
unconstrained parameter behavior, local freshness, laziness and ordinary exported
interfaces. Do not replace the reference or reduce cost assertions. Diagnose
each probe before choosing a repair; source-global runtime work needs a separate
scope decision. Preserve complete-output guards, ordinary caller builds, boundary
and seeded comparisons, and existing optimized postchecks. Promote only cases
that pass the complete lifecycle after explicit review.

## Reproduce

Prepare the pinned runtime using `patches/README.md`, then run each command:

```sh
sh scripts/stack-test.sh --test-arguments=constrained-sharing-all-dictionaries
sh scripts/stack-test.sh --test-arguments=constrained-sharing-constructor-dictionaries
sh scripts/stack-test.sh --test-arguments=constrained-sharing-different-dictionaries
sh scripts/stack-test.sh --test-arguments=constrained-sharing-generic-caller
sh scripts/stack-test.sh --test-arguments=constrained-sharing-nested-allocations
sh scripts/stack-test.sh --test-arguments=constrained-sharing-same-dictionary
sh scripts/stack-test.sh --test-arguments=constrained-sharing-unconstrained-parameter
```
