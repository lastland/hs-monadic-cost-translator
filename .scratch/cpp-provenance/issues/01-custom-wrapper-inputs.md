# Authenticate inputs and options of accepted custom CPP wrappers

Status: needs-info
Type: research

## Contract and current boundary

The global source/output checks apply to accepted translation configurations,
including custom `-pgmP` configurations that genuinely publish on the baseline.
Successful translation does not establish that a later reconstruction has the
same authenticated preprocessing inputs.

The current construction boundary rejects the investigated loaded source-ADT
CPP publication because its frontend buffers do not match exactly. A separate
standard-GHC-CPP prototype has bounded positive and tamper-negative evidence;
it is not integrated and does not establish custom-wrapper support.

## Executed obstruction

An isolated experiment at baseline `3876e4` used a real custom wrapper that:

1. Reads an additional file.
2. Selects one of two child-compiler macro flags from that file.
3. Invokes the real compiler with dependency capture enabled.

Both real translator pipelines published successfully before the unchanged
strict construction guard rejected the buffer mismatch. The two runs had
identical source bytes, wrapper bytes and complete generated publication bytes.
The extra macro was unused, so changing it did not change the program.

The actual child argument lists differed, but genuine compiler dependency files
omitted the wrapper's additional input. Hashing the wrapper and compiler-reported
dependencies therefore cannot establish the required input/option identity.
Wrapper-authored logs were diagnostic evidence, not trusted certificates.

The experiment does not prove that every wrapper is unsafe, that source
semantics changed, or that every sound implementation route is impossible.
It demonstrates that the investigated dependency-capture mechanism is
insufficient on its own.

## Environment limitation

An unprivileged independent tracing probe failed because DTrace required
additional privileges. No privilege changes, sandbox exceptions or runtime
workarounds were attempted. Tracing in a different environment remains
unverified, not an implementation-ready option.

## Acceptance and constraints

Establish an executed custom-wrapper positive through the complete construction
guard and ordinary O0/O2 comparisons. Preserve exact publication bytes, accepted
AST semantics and all original-source term-use rejections. Detect changes to
actual preprocessing inputs, executables and effective options without trusting
an unauthenticated wrapper manifest.

Do not silently waive buffer authentication, normalize arbitrary location or
string content, exempt generated/helper CPP, or admit wrappers based only on
their path or hash. A restricted wrapper policy would require an explicit
scope decision and its own end-to-end evidence.

This issue is separate from the audited reference-failure inventory. It adds no
deferred reference key and does not weaken the default passing suite.

## Evidence

Unpublished `custom-cpp-provenance` artifacts retain the wrapper, annotated Python
probe, both publication logs, genuine dependency files and tracing failure.
Its `receipt.md` documents exact commands and limitations. No custom-wrapper
construction GREEN or production adoption is claimed.
