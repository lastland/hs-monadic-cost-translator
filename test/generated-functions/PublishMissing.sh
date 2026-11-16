#!/bin/sh
# Run real GHC before corrupting only the translator's publication candidate.
"$FUNCTION_SPECIMEN_GHC" "$@"
status=$?
if [ "$status" -ne 0 ]; then
    exit "$status"
fi
for argument do
    case "$argument" in
        */.cost-translate-*/Generated.hs)
            cp "$FUNCTION_SPECIMEN_OUTPUT" "$argument" || exit 1
            printf '%s\n' "$argument" >> "$FUNCTION_SPECIMEN_LOG"
            ;;
    esac
done
