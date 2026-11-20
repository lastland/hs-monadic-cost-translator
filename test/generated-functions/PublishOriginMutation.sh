#!/bin/sh
# Mutate only this fresh lifecycle sandbox after real candidate checking.
set -eu
"$FUNCTION_SPECIMEN_GHC" "$@"
for argument do
    case "$argument" in
        */.cost-translate-*/Generated.hs)
            sandbox=$(dirname "$(dirname "$argument")")
            case "$ORIGIN_SPECIMEN_MUTATION" in
                snapshot)
                    printf '\n-- source changed after frontend acceptance\n' >> "$sandbox/Example.hs"
                    ;;
                options)
                    printf '\n{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}\n' >> "$sandbox/Example.hs"
                    ;;
                module)
                    sed 's/module Example/module WrongOrigin/' "$sandbox/Example.hs" > "$sandbox/Changed.hs"
                    mv "$sandbox/Changed.hs" "$sandbox/Example.hs"
                    ;;
                path)
                    mv "$sandbox/Example.hs" "$sandbox/Unregistered.hs"
                    ln -s Unregistered.hs "$sandbox/Example.hs"
                    ;;
                helper)
                    cp "$ORIGIN_SPECIMEN_HELPER" "$sandbox/OriginHelper.hs"
                    cp "$FUNCTION_SPECIMEN_OUTPUT" "$argument"
                    ;;
                include)
                    printf '#define IDENTITY(x) 0\n' > "$ORIGIN_SPECIMEN_INCLUDE"
                    ;;
                *) exit 1 ;;
            esac
            printf '%s\n' "$argument" >> "$FUNCTION_SPECIMEN_LOG"
            ;;
    esac
done
