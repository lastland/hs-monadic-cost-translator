# Typed diagnostic categories

Status: ready-for-agent

## What to build

Give diagnostics a typed category carrying classification, location, and
detail, replacing the string-prefix and stderr-substring classification the
final standards review flagged. Today a generated-code parse failure can be
labelled unsupported-source because every frontend failure receives the
same prefix, and setup detection matches GHC message text that a compiler
upgrade could drift. Classification should be structural, not
message-based, and the broad exception catch should no longer swallow
async exceptions.

The observable CLI contract stays the same: the four documented categories
(source syntax/type error, unsupported valid source, unavailable target
modules, generated-code failure) with source-located limitation messages
and retained candidates where applicable.

## Acceptance criteria

- [ ] Categories are decided at construction sites, not recovered from
      message text or stderr substrings.
- [ ] Each documented category has a regression pinning its message shape,
      including at least one case that was previously misclassified.
- [ ] Async exceptions propagate; only expected translator failures are
      caught.
- [ ] Full integration suite passes; HLint clean.

## Blocked by

None - can start immediately.
