#!/bin/sh
set -eu
case $0 in
    */*) script_dir=${0%/*} ;;
    *) script_dir=. ;;
esac
# Keep a sentinel after the path so command substitution preserves final LFs.
root=$(CDPATH='' cd -- "$script_dir/.." && printf '%s/' "$PWD")
root=${root%/}
exec python3 "$root/test/source_policy.py" shell "$@"
