# Resolve demand-data construction rejections

Status: needs-triage
Type: task

## Cases and observed diagnostics

Seven exact cases in the [audit map](../map.md) fail construction. For every
selector below the source, caller and unchanged independent reference are
`test/cases/<selector>/Example.hs`, `Case.hs` and `Reference.hs` in that directory.

| Selector | Rejected demand dictionaries | Additional rejection |
|---|---|---|
| `datatypes-and-demands` | `NestedD`, `ThD (NestedD (ListD a))` | `reprM` for `Nested [a]` |
| `nested-pair-and-maybe-fields` | `NestedD`, `ThD (NestedD (PairD a a))` | `reprM` for `Nested (a,a)` |
| `higher-kinded-demands` | Higher-kinded fields, including `OperatedD`, `SelectedD`, `CharacterD`, `NamedD`, `NumericD`, `TagD` | None reported |
| `higher-kinded-polykinded-field` | `HigherD f a`, `ThD (f a)` | None reported |
| `polymorphic-checked-payload-kinds` | `BoxD f`, `ThD (MaybeD (f Int Bool))` | None reported |
| `polymorphic-dependent-payload-kinds` | `IndexD a`, `PacketD f a`, `ThD (MaybeD (f a))` | None reported |
| `polymorphic-vacuous-quantification` | `StampD a` | None reported |

Every case reports `demand-data methods have no observation exemption` for
selected `GHC.Classes.==`, `$dm/=`, `GHC.Internal.Show.showsPrec`, `$dmshow`
and `$dmshowList`. The two nested cases additionally report:

```text
unapproved selected method Memorist.Data.Interface.Data.reprM:
reprM requires a concrete method-specific construction closure and audited carrier
```

These are complete-output construction guard failures, not failed reference
comparisons or evidence that ordinary caller compilation succeeded. Warnings
about `*` in the dependent-payload source are not the rejecting condition.
The growing nested recursion is unsupported by the current closure proof; that
does not make the source inherently unsafe.

## Intended behavior and scope

Approve only genuinely justified method-specific construction and observation
evidence for generated demand data, without blanket Eq/Show exemptions or
unchecked carrier assumptions. Keep finite observations, lazy fields and
polymorphic kind constraints intact. Retain all independent references and costs.

Stock Eq/Show candidate `e577` and closed-local candidate `e34` are not integrated
into the audited `e5d4652` baseline. Their existence does not close any mapped
case, prove the remaining `reprM` obligations, or justify broader certificates.
Re-audit each exact case after any approved integration.

## Acceptance and reproduction

After approved support, pass the complete-output guard, then ordinary caller
compilation, then unchanged boundary/seeded value/cost comparisons and existing
optimized postchecks. A removed first diagnostic is not evidence of full success.
Prepare the runtime using `patches/README.md`, then run each selector:

```sh
sh scripts/stack-test.sh --test-arguments=datatypes-and-demands
sh scripts/stack-test.sh --test-arguments=nested-pair-and-maybe-fields
sh scripts/stack-test.sh --test-arguments=higher-kinded-demands
sh scripts/stack-test.sh --test-arguments=higher-kinded-polykinded-field
sh scripts/stack-test.sh --test-arguments=polymorphic-checked-payload-kinds
sh scripts/stack-test.sh --test-arguments=polymorphic-dependent-payload-kinds
sh scripts/stack-test.sh --test-arguments=polymorphic-vacuous-quantification
```

## Comments

- After reviewed stock integration, the exact
  `higher-kinded-polykinded-field`, `polymorphic-checked-payload-kinds` and
  `polymorphic-dependent-payload-kinds` variants pass their complete ordinary
  reference lifecycles, including O0/O2 boundary cases and 200 randomized
  value/cost comparisons at `88dc779`.
- `polymorphic-vacuous-quantification` also passes its unchanged reference.
  Commit `6f0b1aa` adds this case to the existing optimized postchecks; its fresh
  complete lifecycle passes O0/O2 boundary cases and 200 randomized value/cost
  comparisons. HLint checks pass.
- `higher-kinded-demands` was independently rerun at `758e058` and still fails
  the complete construction guard. The above recoveries do not establish its
  field obligations or either growing-recursion case.
- After split integration, all four recoveries passed again through the
  per-source runner with complete guards, ordinary callers and O0/O2 comparisons.
  Their catalog entries are explicitly removed in the promotion checkpoint;
  the remaining three cases in this issue stay deferred. The historical
  `e5d4652` audit remains unchanged.
