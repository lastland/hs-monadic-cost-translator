#!/bin/sh
# Select one installed toolchain for Cabal and every child ghc invocation.
set -eu

required_ghc=9.10.3
case $0 in
    */*) script_dir=${0%/*} ;;
    *) script_dir=. ;;
esac
# Keep a sentinel after the path so command substitution preserves final LFs.
root=$(CDPATH='' cd -- "$script_dir/.." && printf '%s/' "$PWD")
root=${root%/}

die() {
    printf 'validate: %s\n' "$*" >&2
    exit 1
}

usage() {
    printf '%s\n' \
        'Usage: sh scripts/validate.sh [--check-compiler|--print-compiler]' \
        'Run source, shell, and HLint checks, regressions, and runtime-free tests.' \
        'Use installed GHC 9.10.3; no tools are installed by this script.' \
        'Set GHC to a compiler path or command to override discovery.' \
        'Discovery tries ghc-9.10.3, ghc, then Stack (without installing GHC).'
}

case "$#:${1-}" in
    0:|1:--check-compiler|1:--print-compiler) ;;
    1:--help) usage; exit 0 ;;
    *) usage >&2; exit 1 ;;
esac

# GHCup and other managers often expose versioned symlinks beside a different
# default ghc. Follow the selected link to its installation before setting PATH.
resolve_executable() {
    executable=$1
    while :; do
        directory=$(CDPATH='' cd -P -- "$(dirname -- "$executable")" && pwd) || return 1
        executable="$directory/$(basename -- "$executable")"
        [ -L "$executable" ] || break
        target=$(readlink "$executable") || return 1
        case "$target" in
            /*) executable=$target ;;
            *) executable="$directory/$target" ;;
        esac
    done
    printf '%s\n' "$executable"
}

compiler=
if [ "${GHC+x}" = x ]; then
    compiler=$(command -v "$GHC") || die "GHC is not an executable: $GHC"
else
    for candidate in "ghc-$required_ghc" ghc; do
        candidate=$(command -v "$candidate") || continue
        version=$("$candidate" --numeric-version 2>/dev/null) || continue
        if [ "$version" = "$required_ghc" ]; then
            compiler=$candidate
            break
        fi
    done
    if [ -z "$compiler" ] && command -v stack >/dev/null 2>&1; then
        compiler=$(stack --stack-yaml "$root/scripts/stack-compiler.yaml" --no-install-ghc path --compiler-exe 2>/dev/null) || compiler=
    fi
fi

[ -n "$compiler" ] || die "GHC $required_ghc is required. Install it or set GHC=/path/to/ghc-$required_ghc. No compiler was installed."
[ -x "$compiler" ] || die "compiler is not executable: $compiler"
compiler=$(resolve_executable "$compiler") || die "cannot resolve compiler: $compiler"
[ -x "$compiler" ] || die "compiler is not executable: $compiler"
version=$("$compiler" --numeric-version) || die "cannot query compiler: $compiler"
[ "$version" = "$required_ghc" ] || die "unsupported GHC $version ($compiler); required GHC $required_ghc"

compiler_dir=$(dirname -- "$compiler")
child_ghc="$compiler_dir/ghc"
package_tool="$compiler_dir/ghc-pkg"
[ -x "$child_ghc" ] && [ "$("$child_ghc" --numeric-version)" = "$required_ghc" ] ||
    die "the selected installation must provide ghc $required_ghc in $compiler_dir"
[ -x "$package_tool" ] && [ "$("$package_tool" --version)" = "GHC package manager version $required_ghc" ] ||
    die "the selected installation must provide ghc-pkg $required_ghc in $compiler_dir"

export PATH="$compiler_dir:$PATH"
if [ "${1-}" = --print-compiler ]; then
    printf '%s\n' "$compiler"
    exit 0
fi
printf 'Validating with GHC %s (%s)\n' "$version" "$compiler"
[ "$#" -eq 0 ] || exit 0

command -v cabal >/dev/null 2>&1 || die 'cabal is required for validation'
command -v python3 >/dev/null 2>&1 || die 'python3 is required for validation'
export GHC="$compiler"
cd "$root"
sh "$root/scripts/check-source.sh"
sh "$root/scripts/check-shell.sh"
sh "$root/scripts/check-hlint.sh"
python3 "$root/test/source_policy.py" --ghc "$compiler" --self-test
python3 "$root/test/staged_policy.py" --ghc "$compiler" --self-test
python3 "$root/test/comment_policy.py" --ghc "$compiler" --self-test --check
cabal_with_ghc() {
    cabal "$@" --with-compiler="$compiler" --with-hc-pkg="$package_tool" --ghc-options='-Wall -Werror'
}

cabal_with_ghc build exe:cost-translate test:cost-translate-test
translator=$(cabal_with_ghc list-bin exe:cost-translate)
translator_dir=$(dirname -- "$translator")
export PATH="$translator_dir:$PATH"
cabal_with_ghc test cost-translate-test \
    --test-options='diagnostics generated-module-name-diagnostic whole-module-failure test-selection fixture-policy' \
    --test-show-details=direct
