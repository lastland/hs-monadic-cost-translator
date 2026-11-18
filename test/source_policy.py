#!/usr/bin/env python3
"""Shared, fail-closed source-policy command implementation."""

from __future__ import annotations

import argparse
from collections.abc import Iterator, Mapping, Sequence
from contextlib import contextmanager
from dataclasses import dataclass, field
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tempfile
from typing import cast

from source_policy_metrics import FactsError, FileFacts, decode_facts, measure

ROOT = Path(__file__).resolve().parents[1]
CONFIG = ROOT / "scripts" / "source-policy.json"
FORMAT_PASSES = 16


class PolicyError(Exception):
    pass


@dataclass(frozen=True)
class PathState:
    device: int
    inode: int
    mode: int
    size: int
    modified: int
    changed: int


def path_state(path: Path) -> PathState:
    details = path.lstat()
    return PathState(details.st_dev, details.st_ino, details.st_mode, details.st_size,
                     details.st_mtime_ns, details.st_ctime_ns)


@dataclass(frozen=True)
class SourceTree:
    files: dict[str, str]
    directories: dict[str, int]
    states: dict[str, PathState] = field(default_factory=dict)


@dataclass(frozen=True)
class PolicyConfig:
    tools: dict[str, str]
    limits: dict[str, int]
    shell_exclude: frozenset[str]


@dataclass(frozen=True)
class Engine:
    executable: Path
    libdir: str


@dataclass
class Arguments:
    command: str | None = None
    files: list[str] = field(default_factory=list)
    root: Path = ROOT
    ghc: str | None = None
    self_test: bool = False
    check: bool = False


def object_fields(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise PolicyError("source-policy: configuration: expected object for " + label)
    fields: dict[str, object] = {}
    for key, item in cast(dict[object, object], value).items():
        if not isinstance(key, str):
            raise PolicyError("source-policy: configuration: non-string key in " + label)
        fields[key] = item
    return fields


def load_config() -> PolicyConfig:
    try:
        decoded: object = json.loads(CONFIG.read_bytes().decode("utf-8"))
    except (OSError, UnicodeDecodeError, ValueError) as problem:
        raise PolicyError("scripts/source-policy.json: configuration: cannot read configuration") from problem
    fields = object_fields(decoded, "configuration")
    tools: dict[str, str] = {}
    for key, value in object_fields(fields.get("tools"), "tools").items():
        if not isinstance(value, str) or not value:
            raise PolicyError("source-policy: configuration: expected tool version for " + key)
        tools[key] = value
    if set(tools) != {"ghc", "hlint", "shellcheck", "fourmolu"}:
        raise PolicyError("source-policy: configuration: incomplete tool versions")
    limits: dict[str, int] = {}
    for key, value in object_fields(fields.get("limits"), "limits").items():
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            raise PolicyError("source-policy: configuration: expected nonnegative limit for " + key)
        limits[key] = value
    if set(limits) != {"file_lines", "top_level", "definition_lines", "line_width",
                       "string_occurrences", "directory_entries"}:
        raise PolicyError("source-policy: configuration: incomplete limits")
    exclusions = fields.get("shell_exclude")
    if not isinstance(exclusions, list):
        raise PolicyError("source-policy: configuration: expected shell exclusion list")
    names: set[str] = set()
    for item in cast(list[object], exclusions):
        if not isinstance(item, str) or not item or "/" in item or item in {".", ".."}:
            raise PolicyError("source-policy: configuration: invalid shell exclusion")
        names.add(item)
    return PolicyConfig(tools, limits, frozenset(names))


def child_environment() -> dict[str, str]:
    return dict(os.environ, GHC_ENVIRONMENT="-", SHELLCHECK_OPTS="")


def run_tool(
    command: Sequence[str], root: Path, label: str,
    environment: Mapping[str, str] | None = None, *, timeout: int = 120,
) -> subprocess.CompletedProcess[str]:
    try:
        return subprocess.run(
            list(command), cwd=root,
            env=child_environment() if environment is None else environment,
            capture_output=True, text=True, encoding="utf-8", errors="strict",
            check=False, timeout=timeout,
        )
    except (OSError, UnicodeError, subprocess.TimeoutExpired) as problem:
        raise PolicyError("source-policy: tool: cannot run " + label) from problem


def tool_executable(
    name: str, config: PolicyConfig, root: Path, *, override: str | None = None,
) -> str:
    chosen = override if override is not None else os.environ.get(name.upper(), name)
    executable = shutil.which(chosen)
    if executable is None:
        raise PolicyError("source-policy: tool: " + name + " " + config.tools[name]
                          + " is required (executable not found)")
    argument = "--numeric-version" if name in {"ghc", "hlint"} else "--version"
    result = run_tool([executable, argument], root, name)
    if result.returncode:
        raise PolicyError("source-policy: tool: cannot query " + name + " version")
    version = result.stdout.strip()
    if name == "shellcheck":
        matched = re.search(r"(?m)^version: ([0-9.]+)$", version)
        version = matched.group(1) if matched else "unknown"
    elif name == "fourmolu":
        matched = re.match(r"fourmolu ([0-9.]+)(?:\s|$)", version)
        version = matched.group(1) if matched else "unknown"
    if version != config.tools[name]:
        raise PolicyError("source-policy: tool: unsupported " + name + " " + version
                          + "; required " + config.tools[name])
    return executable


def select_compiler(root: Path, config: PolicyConfig, override: str | None) -> str:
    chosen = override if override is not None else os.environ.get("GHC")
    if chosen is None:
        selected = run_tool(["sh", str(ROOT / "scripts" / "validate.sh"),
                             "--print-compiler"], ROOT, "compiler discovery")
        if selected.returncode:
            raise PolicyError("source-policy: tool: GHC " + config.tools["ghc"]
                              + " discovery failed\n" + selected.stderr.strip())
        chosen = selected.stdout.strip()
    return tool_executable("ghc", config, root, override=chosen)


def display_path(path: str) -> str:
    return path if all(character.isprintable() for character in path) else repr(path)


def discover_source(root: Path) -> SourceTree:
    source = root / "src"
    if source.is_symlink():
        raise PolicyError("src: input: symlinks are unsupported")
    if not source.exists():
        raise PolicyError("src: input: missing source root")
    files: dict[str, str] = {}
    directories: dict[str, int] = {}
    states: dict[str, PathState] = {}
    problems: list[str] = []

    def visit(path: Path) -> None:
        relative = path.relative_to(root).as_posix()
        label = display_path(relative)
        try:
            before = path_state(path)
            mode = before.mode
            if stat.S_ISLNK(mode):
                raise PolicyError("symlinks are unsupported")
            if stat.S_ISDIR(mode):
                if not mode & 0o444 or not mode & 0o111:
                    raise PolicyError("unreadable directory")
                with os.scandir(path) as entries:
                    children = sorted(Path(entry.path) for entry in entries)
                directories[relative] = len(children)
                for child in children:
                    visit(child)
            elif stat.S_ISREG(mode):
                if path.suffix != ".hs":
                    raise PolicyError("unsupported source input (expected .hs)")
                if not mode & 0o444:
                    raise PolicyError("unreadable file")
                files[relative] = path.read_bytes().decode("utf-8", errors="strict")
            else:
                raise PolicyError("unsupported source input (not a regular file)")
            if path_state(path) != before:
                raise PolicyError("changed during source discovery")
            states[relative] = before
        except UnicodeDecodeError:
            problems.append(label + ": input: invalid UTF-8")
        except OSError:
            problems.append(label + ": input: cannot read path")
        except PolicyError as problem:
            problems.append(label + ": input: " + str(problem))

    visit(source)
    if problems:
        raise PolicyError("\n".join(sorted(problems)))
    return SourceTree(dict(sorted(files.items())), dict(sorted(directories.items())), dict(sorted(states.items())))


def require_unchanged(root: Path, original: SourceTree) -> None:
    current = discover_source(root)
    paths = set(original.states) | set(current.states)
    changed = sorted(path for path in paths if original.states.get(path) != current.states.get(path))
    if original.files != current.files or original.directories != current.directories or changed:
        label = display_path(changed[0]) if changed else "src"
        raise PolicyError(label + ": input: source tree changed during policy check")


def discover_shell(root: Path, excluded: frozenset[str]) -> list[str]:
    files: list[str] = []

    def visit(directory: Path) -> None:
        try:
            with os.scandir(directory) as entries:
                children = sorted(Path(entry.path) for entry in entries)
            for path in children:
                relative = display_path(path.relative_to(root).as_posix())
                if path.name in excluded and path.is_dir():
                    continue
                if path.is_symlink():
                    raise PolicyError(relative + ": input: symlinks are unsupported")
                if path.is_dir():
                    visit(path)
                elif path.suffix == ".sh" or path == root / ".githooks" / "pre-commit":
                    mode = path.stat().st_mode
                    if not stat.S_ISREG(mode) or not mode & 0o444:
                        raise PolicyError(relative + ": input: unreadable shell script")
                    files.append(path.relative_to(root).as_posix())
        except OSError as problem:
            label = display_path(directory.relative_to(root).as_posix())
            raise PolicyError(label + ": input: cannot discover shell scripts") from problem

    visit(root)
    return sorted(files)


def format_sources(
    root: Path, config: PolicyConfig, files: Sequence[str], *, inplace: bool = False,
) -> None:
    executable = tool_executable("fourmolu", config, root)
    if inplace:
        stabilize_sources(root, executable, files)
        return
    check_formatting(root, executable, files)


def formatter_command(executable: str, mode: str, path: str, *, strict: bool) -> list[str]:
    return [executable, "--mode", mode, "--config", str(ROOT / "fourmolu.yaml"),
            "--no-cabal", "--ghc-opt=-XGHC2021", *(["--check-idempotence"] if strict else []),
            "--quiet", "--color", "never", "--", path]


def check_formatting(root: Path, executable: str, files: Sequence[str]) -> None:
    problems: list[str] = []
    for path in files:
        result = run_tool(
            formatter_command(executable, "check", path, strict=True), root, "fourmolu",
        )
        if result.returncode:
            reason = ("not Fourmolu-clean" if result.returncode == 100
                      else "Fourmolu failed (exit " + str(result.returncode) + ")")
            problems.append(display_path(path) + ": formatting: " + reason)
            if result.returncode != 100:
                detail = (result.stdout + result.stderr).strip()
                detail = detail.replace(str(root), ".").replace(str(ROOT), ".")
                if detail:
                    problems.append(detail)
    if problems:
        raise PolicyError("\n".join(problems))


def stabilize_sources(root: Path, executable: str, files: Sequence[str]) -> None:
    """Stage safe AST-checked passes; publish nothing until every file is stable."""
    original = discover_source(root)
    with tempfile.TemporaryDirectory(prefix="source-policy-format-") as temporary:
        stage = Path(temporary)
        outputs: dict[str, bytes] = {}
        for path in files:
            if path not in original.files:
                raise PolicyError(display_path(path) + ": input: select a src .hs file")
            candidate = original.files[path].encode("utf-8")
            seen = {candidate}
            destination = stage / path
            destination.parent.mkdir(parents=True, exist_ok=True)
            for _ in range(FORMAT_PASSES):
                destination.write_bytes(candidate)
                result = run_tool(formatter_command(executable, "stdout", path, strict=False), stage, "fourmolu")
                if result.returncode:
                    detail = (result.stdout + result.stderr).strip().replace(str(stage), ".").replace(str(ROOT), ".")
                    raise PolicyError(display_path(path) + ": formatting: Fourmolu failed (exit "
                                      + str(result.returncode) + ")" + ("\n" + detail if detail else ""))
                output = result.stdout.encode("utf-8")
                if output == candidate:
                    outputs[path] = output
                    break
                if output in seen:
                    raise PolicyError(display_path(path) + ": formatting: cycle detected")
                seen.add(output)
                candidate = output
            else:
                raise PolicyError(display_path(path) + ": formatting: did not converge within "
                                  + str(FORMAT_PASSES) + " safe passes")
        check_formatting(stage, executable, files)
        require_unchanged(root, original)
        for path, output in outputs.items():
            if output != original.files[path].encode("utf-8"):
                try:
                    target = root / path
                    if path_state(target) != original.states[path] or target.read_bytes() != original.files[path].encode("utf-8"):
                        raise PolicyError(display_path(path) + ": input: source changed before formatted output could be written")
                    descriptor = os.open(target, os.O_RDWR | os.O_NOFOLLOW)
                    with os.fdopen(descriptor, "r+b") as destination:
                        if destination.read() != original.files[path].encode("utf-8") or path_state(target) != original.states[path]:
                            raise PolicyError(display_path(path) + ": input: source changed before formatted output could be written")
                        destination.seek(0)
                        destination.write(output)
                        destination.truncate()
                except OSError as problem:
                    raise PolicyError(display_path(path) + ": formatting: cannot write formatted source") from problem


def check_source(root: Path, config: PolicyConfig, compiler: str | None = None) -> None:
    tree = discover_source(root)
    format_sources(root, config, list(tree.files))
    require_unchanged(root, tree)
    selected = select_compiler(root, config, compiler)
    facts: dict[str, FileFacts] = {}
    if tree.files:
        with build_engine(selected) as engine:
            facts = collect_facts(root, tree, engine)
    enforce_metrics(root, config, tree, facts)
    print("Source policy: checked " + str(len(tree.files)) + " source files")


def tool_failure(label: str, result: subprocess.CompletedProcess[str], *roots: Path) -> PolicyError:
    detail = (result.stderr + result.stdout).strip()
    for root in roots:
        detail = detail.replace(str(root), ".")
    return PolicyError("source-policy: tool: " + label + " failed"
                       + ("\n" + detail if detail else ""))


@contextmanager
def build_engine(compiler: str) -> Iterator[Engine]:
    with tempfile.TemporaryDirectory(prefix="source-policy-build-") as temporary:
        directory = Path(temporary)
        executable = directory / "source-policy"
        result = run_tool(
            [compiler, "-v0", "--make", "-dynamic", "-package", "ghc", "-Wall", "-Werror",
             "-main-is", "SourcePolicy.main", "-i" + str(ROOT / "test" / "source-policy-engine"),
             "-outputdir", str(directory), "-o", str(executable), str(ROOT / "test" / "SourcePolicy.hs")],
            ROOT, "GHC checker build",
        )
        if result.returncode:
            raise tool_failure("GHC checker build", result, directory, ROOT)
        library = run_tool([compiler, "--print-libdir"], ROOT, "GHC library directory")
        if library.returncode or not library.stdout.strip():
            raise tool_failure("GHC library directory", library, directory, ROOT)
        yield Engine(executable, library.stdout.strip())


def collect_facts(root: Path, tree: SourceTree, engine: Engine) -> dict[str, FileFacts]:
    require_unchanged(root, tree)
    result = run_tool([str(engine.executable), engine.libdir, *sorted(tree.files)], root, "GHC facts engine")
    require_unchanged(root, tree)
    if result.returncode:
        raise tool_failure("GHC facts engine", result, engine.executable.parent, root, ROOT)
    try:
        return decode_facts(result.stdout, tree.files)
    except FactsError as problem:
        raise PolicyError("source-policy: facts: " + str(problem)) from problem


def enforce_metrics(root: Path, config: PolicyConfig, tree: SourceTree, facts: Mapping[str, FileFacts]) -> None:
    violations = measure(tree.files, tree.directories, facts, config.limits)
    require_unchanged(root, tree)
    if violations:
        raise PolicyError("\n".join(violation.render() for violation in violations))


def run_self_tests(config: PolicyConfig, compiler: str | None) -> str:
    selected = select_compiler(ROOT, config, compiler)
    for name in ("fourmolu", "hlint", "shellcheck"):
        tool_executable(name, config, ROOT)
    for entry in ("source_policy_tests.py", "source_policy_engine.py"):
        result = run_tool([sys.executable, str(ROOT / "test" / entry), "--ghc", selected,
                           *(["--self-test"] if entry == "source_policy_engine.py" else [])],
                          ROOT, entry + " self-tests", timeout=600)
        sys.stdout.write(result.stdout)
        sys.stderr.write(result.stderr)
        if result.returncode:
            raise PolicyError("source-policy: self-test: " + entry + " failed")
    return selected


def format_selected(root: Path, config: PolicyConfig, selected: Sequence[str]) -> None:
    tree = discover_source(root)
    files: list[str] = []
    for name in selected:
        path = Path(name)
        try:
            relative = (root / path).resolve().relative_to(root.resolve()).as_posix()
        except (OSError, ValueError) as problem:
            raise PolicyError("source-policy: input: selected path is outside src") from problem
        if relative not in tree.files:
            raise PolicyError(display_path(relative) + ": input: select a src .hs file")
        files.append(relative)
    chosen = sorted(set(files)) if selected else list(tree.files)
    format_sources(root, config, chosen, inplace=True)
    format_sources(root, config, chosen)
    print("Fourmolu: formatted " + str(len(chosen)) + " source files")


def check_shell(root: Path, config: PolicyConfig) -> None:
    files = discover_shell(root, config.shell_exclude)
    executable = tool_executable("shellcheck", config, root)
    if files:
        result = run_tool([executable, "--rcfile=" + str(ROOT / ".shellcheckrc"),
                           "--format=gcc", "--color=never", "--", *files], root, "shellcheck")
        if result.returncode:
            raise tool_failure("shellcheck", result, root, ROOT)
    print("ShellCheck: checked " + str(len(files)) + " maintained shell files")


def discover_hlint(root: Path) -> list[str]:
    files = list(discover_source(root).files)

    def visit(path: Path) -> None:
        label = display_path(path.relative_to(root).as_posix())
        try:
            if path.is_symlink():
                raise PolicyError(label + ": input: symlinks are unsupported")
            if path.is_dir():
                with os.scandir(path) as entries:
                    children = sorted(Path(entry.path) for entry in entries)
                for child in children:
                    visit(child)
            elif path.suffix in {".hs", ".lhs"}:
                if not path.is_file() or not path.stat().st_mode & 0o444:
                    raise PolicyError(label + ": input: unreadable Haskell file")
                files.append(path.relative_to(root).as_posix())
        except OSError as problem:
            raise PolicyError(label + ": input: cannot discover Haskell files") from problem

    if not (root / "app").is_dir():
        raise PolicyError("app: input: missing HLint source directory")
    visit(root / "app")
    for directory in ("test", "test/reference"):
        if not (root / directory).is_dir():
            raise PolicyError(directory + ": input: missing HLint source directory")
        for path in sorted((root / directory).glob("*.hs")):
            visit(path)
    helpers = root / "test" / "source-policy-engine"
    if helpers.exists():
        visit(helpers)
    return sorted(set(files))


def check_hlint(root: Path, config: PolicyConfig) -> None:
    executable = tool_executable("hlint", config, root)
    files = discover_hlint(root)
    if not files:
        raise PolicyError("source-policy: input: no HLint source files")
    result = run_tool([executable, "--hint=" + str(ROOT / ".hlint.yaml"),
                       "--colour=never", "--", *files], root, "hlint")
    if result.returncode:
        raise tool_failure("hlint", result, root, ROOT)
    print("HLint: checked " + str(len(files)) + " Haskell files (including suggestions)")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", nargs="?", choices=["check", "format", "shell", "hlint"])
    parser.add_argument("files", nargs="*")
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--ghc")
    parser.add_argument("--self-test", action="store_true", help="run checker, script, and GHC engine regressions")
    parser.add_argument("--check", action="store_true", help="also check sources after self-tests")
    arguments = parser.parse_args(namespace=Arguments())
    if arguments.files and arguments.command != "format":
        parser.error("only format accepts selected file paths")
    if arguments.self_test and arguments.command:
        parser.error("--self-test does not take a command; use --check to scan after tests")
    if arguments.check and arguments.command:
        parser.error("--check does not take a command")
    try:
        config = load_config()
        if arguments.self_test:
            selected = run_self_tests(config, arguments.ghc)
            if arguments.check:
                check_source(arguments.root, config, selected)
        elif arguments.command == "shell":
            check_shell(arguments.root, config)
        elif arguments.command == "hlint":
            check_hlint(arguments.root, config)
        elif arguments.command == "format":
            format_selected(arguments.root, config, arguments.files)
        else:
            check_source(arguments.root, config, arguments.ghc)
    except PolicyError as problem:
        print(problem, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
