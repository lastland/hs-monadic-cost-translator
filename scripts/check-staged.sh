#!/bin/sh
# Bootstrap the staged implementation, not an unstaged working-tree helper.
set -eu
export GIT_OPTIONAL_LOCKS=0 GIT_NO_REPLACE_OBJECTS=1

# Direct invocation captures the same index inode as the hook. An invocation
# from the hook reuses its descriptor instead of reopening the live index.
if [ -z "${SOURCE_POLICY_ORIGINAL_INDEX-}" ]; then
    index=$(git rev-parse --path-format=absolute --git-path index && printf '.')
    index=${index%.}
    index=${index%?}
    exec 3< "$index"
    SOURCE_POLICY_INDEX_STAMP=$(python3 -I -c '
import os
state: os.stat_result = os.fstat(3)
print(repr((state.st_dev, state.st_ino, state.st_mode, state.st_size,
            state.st_mtime_ns, state.st_ctime_ns)))
')
    export SOURCE_POLICY_INDEX_STAMP
    export SOURCE_POLICY_ORIGINAL_INDEX="$index"
fi
export GIT_INDEX_FILE=/dev/fd/3

entry=$(git ls-files --stage -- ':(top)scripts/staged_policy.py')
case "${entry%% *}" in
    100644|100755) ;;
    *) printf '%s\n' 'staged-policy: missing or unsupported staged scripts/staged_policy.py' >&2; exit 1 ;;
esac
implementation=$(git show :scripts/staged_policy.py)
[ -n "$implementation" ] || { printf '%s\n' 'staged-policy: empty staged bootstrap: scripts/staged_policy.py' >&2; exit 1; }
exec python3 -I -c "$implementation" "$@"
