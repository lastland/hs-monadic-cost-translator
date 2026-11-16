#!/bin/sh
# Retain verbose Stack output without flooding the calling terminal.
set -eu

log=$(mktemp "${TMPDIR:-/tmp}/stack-test.XXXXXX")
if stack test "$@" >"$log" 2>&1; then
    printf 'PASS\nLog: %s\n' "$log"
    exit 0
else
    status=$?
fi

printf 'FAIL (exit %s)\nLog: %s\n' "$status" "$log"
# Bound both long logs and long lines; display errors must not replace Stack's status.
tail -n 60 "$log" 2>/dev/null | tail -c 12000 2>/dev/null || :
exit "$status"
