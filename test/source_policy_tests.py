#!/usr/bin/env python3
"""Regression tests for the shared source-policy commands."""

from __future__ import annotations

import argparse
from collections.abc import Mapping
from dataclasses import dataclass
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile
from typing import TypedDict
import unittest
from unittest.mock import patch

from source_policy import SourceTree, discover_source, load_config, run_tool
from source_policy_metrics import FactsError, decode_facts, measure


ROOT = Path(__file__).resolve().parents[1]
DRIVER = ROOT / "test" / "source_policy.py"


class ProtocolNamed(TypedDict):
    line: int
    column: int
    name: str


class ProtocolDefinition(ProtocolNamed):
    lines: int


class ProtocolString(TypedDict):
    line: int
    column: int
    value: str


class ProtocolFile(TypedDict):
    path: str
    code_lines: list[int]
    top_level: list[ProtocolNamed]
    definitions: list[ProtocolDefinition]
    strings: list[ProtocolString]


def protocol_file(path: str, count: int = 0) -> ProtocolFile:
    return {"path": path, "code_lines": list(range(1, count + 1)),
            "top_level": [], "definitions": [], "strings": []}


def report(files: Mapping[str, str], records: list[ProtocolFile], directories: Mapping[str, int] | None = None) -> list[str]:
    facts = decode_facts("\n".join(json.dumps(record) for record in records), files)
    return [violation.render() for violation in measure(files, directories or {"src": len(files)}, facts, load_config().limits)]


@dataclass
class TestArguments:
    ghc: str | None = None


def sized_function(name: str, local_count: int) -> str:
    return (name + " :: Int\n" + name + " = local0\n    where\n"
            + "\n".join("        local" + str(number) + " = " + str(number)
                        for number in range(local_count)))


class SourcePolicyCommands(unittest.TestCase):
    root: Path

    def setUp(self) -> None:
        temporary = tempfile.TemporaryDirectory(prefix="source policy ")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)

    def test_explicit_empty_tool_environment_does_not_inherit_parent(self) -> None:
        command = [sys.executable, "-c", "import os; print(os.environ.get('SOURCE_POLICY_ENVIRONMENT_FIXTURE', 'missing'))"]
        with patch.dict(os.environ, {"SOURCE_POLICY_ENVIRONMENT_FIXTURE": "visible"}):
            isolated = run_tool(command, self.root, "environment fixture", environment={})
            inherited = run_tool(command, self.root, "environment fixture")
        self.assertEqual(isolated.returncode, 0, isolated.stderr)
        self.assertEqual(isolated.stdout, "missing\n")
        self.assertEqual(inherited.returncode, 0, inherited.stderr)
        self.assertEqual(inherited.stdout, "visible\n")

    def test_linter_failures_do_not_publish_temporary_checkout_paths(self) -> None:
        for directory in ("src", "app", "test/reference"):
            (self.root / directory).mkdir(parents=True, exist_ok=True)
        (self.root / "src/Example.hs").write_text("module Example where\n")
        (self.root / "script.sh").write_text("#!/bin/sh\nexit 0\n")
        for name, command, option, version, configuration in (
            ("shellcheck", "shell", "--version", "version: 0.11.0", ".shellcheckrc"),
            ("hlint", "hlint", "--numeric-version", "3.10", ".hlint.yaml"),
        ):
            with self.subTest(tool=name):
                tool = self.root / ("failing " + name)
                tool.write_text(
                    '#!/bin/sh\nif [ "$1" = "' + option + '" ]; then\n'
                    '    printf "%s\\n" "' + version + '"\n    exit 0\nfi\n'
                    'printf "%s/' + configuration + ': invalid configuration\\n" "$PWD"\n'
                    'exit 1\n'
                )
                tool.chmod(0o755)
                result = self.command(command, environment={name.upper(): str(tool)})
                self.assertEqual(result.returncode, 1)
                self.assertIn(configuration + ": invalid configuration", result.stderr)
                self.assertNotIn(str(self.root), result.stderr)

    def command(
        self, *arguments: str, environment: Mapping[str, str] | None = None
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(DRIVER), *arguments, "--root", str(self.root)],
            env=dict(os.environ, **(environment or {})),
            capture_output=True,
            text=True,
            check=False,
            timeout=120,
        )

    def script(
        self, name: str, *arguments: str, environment: Mapping[str, str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["sh", str(ROOT / "scripts" / name), *arguments, "--root", str(self.root)],
            env=dict(os.environ, **(environment or {})), capture_output=True, text=True, check=False, timeout=120,
        )

    def compiler_fixture(self, records: list[ProtocolFile], *, engine_code: str = "") -> Path:
        compiler = self.root / "compiler fixture"
        engine = ("#!/usr/bin/env python3\n" + engine_code + "\n"
                  + "print(" + repr("\n".join(json.dumps(record) for record in records)) + ")\n")
        compiler.write_text(
            '#!/usr/bin/env python3\nimport pathlib\nimport sys\n'
            'if "--numeric-version" in sys.argv:\n    print("9.10.3")\n'
            'elif "--print-libdir" in sys.argv:\n    print("fixture-library")\n'
            'else:\n    output: pathlib.Path = pathlib.Path(sys.argv[sys.argv.index("-o") + 1])\n'
            '    output.write_text(' + repr(engine) + ')\n    output.chmod(0o755)\n'
        )
        compiler.chmod(0o755)
        return compiler

    def test_missing_source_root_is_an_explicit_input_failure(self) -> None:
        result = self.command("check")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "src: input: missing source root\n")

    def test_source_symlinks_are_rejected_not_followed(self) -> None:
        (self.root / "outside").mkdir()
        (self.root / "src").symlink_to(self.root / "outside")
        result = self.command("check")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "src: input: symlinks are unsupported\n")

    def test_discovery_includes_untracked_hidden_nested_sources_without_git(self) -> None:
        for name in ("src/Z.hs", "src/.hidden/A.hs", "src/Nested/B.hs"):
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("module Example where\nvalue = 1\n")
        for name in ("test/Bad.hs", "app/Bad.hs", "examples/Bad.hs"):
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(b"\xff")
        tree = discover_source(self.root)
        self.assertEqual(list(tree.files), ["src/.hidden/A.hs", "src/Nested/B.hs", "src/Z.hs"])
        self.assertEqual(tree.directories, {"src": 3, "src/.hidden": 1, "src/Nested": 1})

    def test_discovered_file_order_is_repository_path_order_not_creation_order(self) -> None:
        for name in ("src/A/Inside.hs", "src/A.hs", "src/B.hs"):
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("module Example where\n")
        self.assertEqual(list(discover_source(self.root).files), ["src/A.hs", "src/A/Inside.hs", "src/B.hs"])

    def test_new_maintained_shell_scripts_are_checked_without_git(self) -> None:
        path = self.root / "new scripts" / "check.sh"
        path.parent.mkdir()
        original = '#!/bin/sh\nargument="two words"\nprintf "%s\\n" $argument\n'
        path.write_text(original)
        result = self.command("shell")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("new scripts/check.sh:3:", result.stderr)
        self.assertIn("SC2086", result.stderr)
        self.assertEqual(path.read_text(), original)

    def test_extensionless_maintained_pre_commit_hook_is_shellchecked(self) -> None:
        path = self.root / ".githooks" / "pre-commit"
        path.parent.mkdir()
        original = '#!/bin/sh\nargument="two words"\nprintf "%s\\n" $argument\n'
        path.write_text(original)
        result = self.script("check-shell.sh")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn(".githooks/pre-commit:3:", result.stderr)
        self.assertIn("SC2086", result.stderr)
        self.assertEqual(path.read_text(), original)

    def test_wrong_formatter_version_fails_before_inspecting_metrics(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        source.write_text("module Example where\nvalue = 1\n")
        tool = self.root / "wrong formatter"
        tool.write_text('#!/bin/sh\nprintf "fourmolu 0.0.0.0\\n"\n')
        tool.chmod(0o755)
        result = self.command("check", environment={"FOURMOLU": str(tool)})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "source-policy: tool: unsupported fourmolu 0.0.0.0; required 0.21.0.0\n")

    def test_wrong_hlint_version_is_not_accepted(self) -> None:
        tool = self.root / "wrong linter"
        tool.write_text('#!/bin/sh\nprintf "3.5\\n"\n')
        tool.chmod(0o755)
        result = self.command("hlint", environment={"HLINT": str(tool)})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "source-policy: tool: unsupported hlint 3.5; required 3.10\n")

    def test_hlint_script_keeps_all_existing_coverage_and_ignores_global_suppression(self) -> None:
        names = ("src/Example.hs", "app/Main.hs", "test/Probe.hs", "test/reference/Reference.hs")
        for name in names:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("module Example where\n\nvalue = id (1 :: Int)\n")
        outside = self.root / "test" / "specimens" / "NotChecked.hs"
        outside.parent.mkdir()
        outside.write_bytes(b"\xff")
        home = self.root / "home"
        home.mkdir()
        (home / ".hlint.yaml").write_text('- ignore: {name: "Redundant id"}\n')
        failed = self.script("check-hlint.sh", environment={"HOME": str(home)})
        self.assertEqual(failed.returncode, 1, failed.stdout + failed.stderr)
        self.assertIn("Redundant id", failed.stderr)
        for name in names:
            self.assertIn(name + ":3:", failed.stderr)
            (self.root / name).write_text("module Example where\n\nvalue :: Int\nvalue = 1\n")
        accepted = self.script("check-hlint.sh", environment={"HOME": str(home)})
        self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
        self.assertEqual(accepted.stdout, "HLint: checked 4 Haskell files (including suggestions)\n")

    def test_shellcheck_user_options_cannot_suppress_project_diagnostics(self) -> None:
        path = self.root / "script.sh"
        path.write_text('#!/bin/sh\nargument="two words"\nprintf "%s\\n" $argument\n')
        result = self.command("shell", environment={"SHELLCHECK_OPTS": "--exclude=SC2086"})
        self.assertEqual(result.returncode, 1)
        self.assertIn("SC2086", result.stderr)

    def test_shellcheck_uses_each_declared_shell_and_excludes_builds(self) -> None:
        (self.root / "bash.sh").write_text('#!/bin/bash\nitems=(one two)\nprintf "%s\\n" "${items[@]}"\n')
        for directory in (".git", ".scratch", ".stack-work", "dist", "dist-newstyle", "hs-pbt"):
            path = self.root / directory / "ignored.sh"
            path.parent.mkdir()
            path.write_text("not a shell script\n")
        result = self.command("shell")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "ShellCheck: checked 1 maintained shell files\n")

    def test_invalid_source_inputs_fail_with_physical_repository_paths(self) -> None:
        specimens: list[tuple[str, bytes, str]] = [
            ("Bad.hs", b"\xff", "invalid UTF-8"),
            ("Other.lhs", b"module Other where\n", "unsupported source input (expected .hs)"),
        ]
        directory = self.root / "src"
        directory.mkdir()
        for name, content, reason in specimens:
            with self.subTest(name=name):
                path = directory / name
                path.write_bytes(content)
                try:
                    result = self.command("check")
                    self.assertEqual(result.returncode, 1)
                    self.assertEqual(result.stderr, "src/" + name + ": input: " + reason + "\n")
                finally:
                    path.unlink()

    def test_nested_broken_symlinks_are_rejected(self) -> None:
        directory = self.root / "src" / "Nested"
        directory.mkdir(parents=True)
        (directory / "Broken.hs").symlink_to(self.root / "absent")
        result = self.command("check")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "src/Nested/Broken.hs: input: symlinks are unsupported\n")

    def test_unreadable_source_is_not_reported_as_compliant(self) -> None:
        path = self.root / "src" / "Private.hs"
        path.parent.mkdir()
        path.write_text("module Private where\n")
        path.chmod(0)
        self.addCleanup(path.chmod, 0o644)
        result = self.command("check")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "src/Private.hs: input: unreadable file\n")

    def test_explicit_wrong_compiler_is_rejected_without_fallback(self) -> None:
        (self.root / "src").mkdir()
        formatter = self.root / "formatter version fixture"
        formatter.write_text('#!/bin/sh\nprintf "fourmolu 0.21.0.0\\n"\n')
        formatter.chmod(0o755)
        compiler = self.root / "wrong compiler"
        compiler.write_text('#!/bin/sh\nprintf "9.14.1\\n"\n')
        compiler.chmod(0o755)
        result = self.command("check", environment={"FOURMOLU": str(formatter), "GHC": str(compiler)})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "source-policy: tool: unsupported ghc 9.14.1; required 9.10.3\n")

    def test_physical_line_width_uses_codepoints_tabs_and_crlf(self) -> None:
        text = "-- " + "é" * 77 + "\r\n" + "\t" * 10 + " \r\n" + "-- final"
        tree = SourceTree({"src/Width.hs": text}, {"src": 1})
        facts = decode_facts(
            '{"path":"src/Width.hs","code_lines":[],"top_level":[],"definitions":[],"strings":[]}',
            tree.files,
        )
        self.assertEqual(
            [violation.render() for violation in measure(tree.files, tree.directories, facts, load_config().limits)],
            ["src/Width.hs:2:81: line_width: actual 81, limit 80"],
        )

    def test_width_preserves_trailing_space_and_last_line_without_newline(self) -> None:
        path = "src/Width.hs"
        for delimiter in ("\n", "\r\n"):
            for final_newline in ("", delimiter):
                with self.subTest(delimiter=repr(delimiter), final_newline=bool(final_newline)):
                    source = "-- " + "🙂" * 76 + " " + delimiter + "-- " + "🙂" * 76 + "  " + final_newline
                    self.assertEqual(report({path: source}, [protocol_file(path)]),
                                     ["src/Width.hs:2:81: line_width: actual 81, limit 80"])

    def test_file_code_line_limit_is_inclusive_and_uses_physical_location(self) -> None:
        path = "src/Count.hs"
        self.assertEqual(report({path: "x\n" * 500}, [protocol_file(path, 500)]), [])
        self.assertEqual(report({path: "x\n" * 501}, [protocol_file(path, 501)]),
                         ["src/Count.hs:501:1: file_lines: actual 501, limit 500"])

    def test_top_level_limit_is_inclusive_and_orders_declarations_by_position(self) -> None:
        path = "src/Count.hs"
        for count in (20, 21):
            record = protocol_file(path, count)
            record["top_level"] = [{"line": number, "column": 1, "name": "value" + str(number)}
                                   for number in reversed(range(1, count + 1))]
            expected = [] if count == 20 else ["src/Count.hs:21:1: top_level: actual 21, limit 20"]
            self.assertEqual(report({path: "value = 1\n" * count}, [record]), expected)

    def test_definition_limit_is_inclusive_and_keeps_nested_physical_position(self) -> None:
        path = "src/Nested.hs"
        for count in (50, 51):
            record = protocol_file(path, count)
            record["definitions"] = [{"line": 3, "column": 5, "name": "inner", "lines": count}]
            expected = [] if count == 50 else ["src/Nested.hs:3:5: definition_lines: actual 51, limit 50"]
            self.assertEqual(report({path: "    value = 1\n" * count}, [record]), expected)

    def test_decoded_string_limit_includes_empty_and_short_literals(self) -> None:
        path = "src/Strings.hs"
        for value in ("", "a", "a longer value"):
            for count in (2, 3):
                with self.subTest(value=value, count=count):
                    record = protocol_file(path, count)
                    record["strings"] = [{"line": number, "column": 9, "value": value}
                                         for number in reversed(range(1, count + 1))]
                    expected = [] if count == 2 else ["src/Strings.hs:3:9: string_occurrences: actual 3, limit 2"]
                    self.assertEqual(report({path: 'value = "text"\n' * count}, [record]), expected)

    def test_directory_limit_counts_only_immediate_mixed_and_hidden_entries(self) -> None:
        source = self.root / "src"
        source.mkdir()
        for number in range(10):
            (source / ("Value" + str(number) + ".hs")).write_text("module Value where\n")
            (source / (".hidden" + str(number))).mkdir()
        tree = discover_source(self.root)
        records = [protocol_file(path) for path in tree.files]
        self.assertEqual(report(tree.files, records, tree.directories), [])
        (source / ".one more directory").mkdir()
        tree = discover_source(self.root)
        self.assertEqual(report(tree.files, records, tree.directories),
                         ["src:1:1: directory_entries: actual 21, limit 20"])

    def test_format_cycle_fails_without_rewriting_selected_sources(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = "module Example where\n\nvalue = 1\n"
        source.write_text(original)
        formatter = self.root / "cycling formatter"
        formatter.write_text(
            '#!/usr/bin/env python3\n'
            'import pathlib\nimport sys\n'
            'if "--version" in sys.argv:\n'
            '    print("fourmolu 0.21.0.0")\n'
            'elif sys.argv[sys.argv.index("--mode") + 1] == "stdout":\n'
            '    text = pathlib.Path(sys.argv[-1]).read_text()\n'
            '    print(text.replace("value = 1", "value = 2") if "value = 1" in text '
            'else text.replace("value = 2", "value = 1"), end="")\n'
        )
        formatter.chmod(0o755)
        result = self.command("format", environment={"FOURMOLU": str(formatter)})
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("src/Example.hs: formatting: cycle detected", result.stderr)
        self.assertEqual(source.read_text(), original)

    def test_format_script_formats_only_selected_source_after_strict_check(self) -> None:
        selected = self.root / "src" / "folder with spaces" / "Example.hs"
        selected.parent.mkdir(parents=True)
        selected.write_text("module Example where\nvalue=1\n")
        other = self.root / "src" / "Other.hs"
        original = "module Other where\nother=2\n"
        other.write_text(original)
        result = self.script("format-source.sh", "src/folder with spaces/Example.hs")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(selected.read_bytes(), b"module Example where\n\nvalue = 1\n")
        self.assertEqual(other.read_text(), original)
        again = self.script("format-source.sh", "src/folder with spaces/Example.hs")
        self.assertEqual(again.returncode, 0, again.stdout + again.stderr)
        self.assertEqual(selected.read_bytes(), b"module Example where\n\nvalue = 1\n")

    def test_real_formatter_failure_does_not_partially_rewrite_selection(self) -> None:
        directory = self.root / "src"
        directory.mkdir()
        original = "module A where\nvalue=1\n"
        (directory / "A.hs").write_text(original)
        malformed = "module Z where\nvalue = (\n"
        (directory / "Z.hs").write_text(malformed)
        result = self.script("format-source.sh")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("src/Z.hs: formatting: Fourmolu failed", result.stderr)
        self.assertNotIn("source-policy-format-", result.stderr)
        self.assertEqual((directory / "A.hs").read_text(), original)
        self.assertEqual((directory / "Z.hs").read_text(), malformed)

    def test_nonconverging_formatter_is_bounded_without_rewriting_input(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = "module Example where\n\nvalue = 1\n"
        source.write_text(original)
        formatter = self.root / "nonconverging formatter"
        formatter.write_text(
            '#!/usr/bin/env python3\nfrom pathlib import Path\nimport sys\n'
            'if "--version" in sys.argv:\n    print("fourmolu 0.21.0.0")\n'
            'else:\n    print(Path(sys.argv[-1]).read_text() + "\\n", end="")\n'
        )
        formatter.chmod(0o755)
        result = self.command("format", environment={"FOURMOLU": str(formatter)})
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("src/Example.hs: formatting: did not converge", result.stderr)
        self.assertEqual(source.read_text(), original)

    def test_final_strict_check_failure_publishes_no_staged_formatting(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = "module Example where\nvalue=1\n"
        source.write_text(original)
        formatter = self.root / "strict check failure formatter"
        formatter.write_text(
            '#!/usr/bin/env python3\nfrom pathlib import Path\nimport sys\n'
            'if "--version" in sys.argv:\n    print("fourmolu 0.21.0.0")\n'
            'elif sys.argv[sys.argv.index("--mode") + 1] == "check":\n    sys.exit(101)\n'
            'else:\n    print(Path(sys.argv[-1]).read_text().replace("value=1", "value = 1"), end="")\n'
        )
        formatter.chmod(0o755)
        result = self.command("format", environment={"FOURMOLU": str(formatter)})
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "src/Example.hs: formatting: Fourmolu failed (exit 101)\n")
        self.assertEqual(source.read_text(), original)

    def test_external_edit_during_stabilization_is_not_clobbered(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        source.write_text("module Example where\nvalue=1\n")
        replacement = "module Example where\n\nnewValue = 2\n"
        formatter = self.root / "concurrent editor formatter fixture"
        formatter.write_text(
            '#!/usr/bin/env python3\nfrom pathlib import Path\nimport sys\n'
            'if "--version" in sys.argv:\n    print("fourmolu 0.21.0.0")\n'
            'elif sys.argv[sys.argv.index("--mode") + 1] == "stdout":\n'
            '    Path(' + repr(str(source)) + ').write_text(' + repr(replacement) + ')\n'
            '    print(Path(sys.argv[-1]).read_text().replace("value=1", "value = 1"), end="")\n'
        )
        formatter.chmod(0o755)
        result = self.command("format", environment={"FOURMOLU": str(formatter)})
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("source tree changed during policy check", result.stderr)
        self.assertEqual(source.read_text(), replacement)

    def test_real_unformatted_input_fails_before_compiler_or_metrics_are_called(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = b"module Example where\nvalue=1\n"
        source.write_bytes(original)
        compiler = self.root / "must not be called"
        marker = self.root / "compiler was called"
        compiler.write_text('#!/bin/sh\ntouch "' + str(marker) + '"\nexit 1\n')
        compiler.chmod(0o755)
        result = self.script("check-source.sh", "--ghc", str(compiler))
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "src/Example.hs: formatting: not Fourmolu-clean\n")
        self.assertFalse(marker.exists())
        self.assertEqual(source.read_bytes(), original)

    def test_formatted_input_reaches_shared_engine_without_editing_source(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = b"module Example where\n\nvalue = 1\n"
        source.write_bytes(original)
        outside = self.root / "test" / "Outside.hs"
        outside.parent.mkdir()
        outside.write_bytes(b"\xff" + b"x" * 1000)
        record = protocol_file("src/Example.hs")
        record["code_lines"] = [1, 3]
        record["top_level"] = [{"line": 3, "column": 1, "name": "value"}]
        record["definitions"] = [{"line": 3, "column": 1, "name": "value", "lines": 1}]
        marker = self.root / "engine was called"
        compiler = self.compiler_fixture([record], engine_code="from pathlib import Path\nPath(" + repr(str(marker)) + ").write_text('ran')")
        result = self.script("check-source.sh", "--ghc", str(compiler))
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "Source policy: checked 1 source files\n")
        self.assertTrue(marker.is_file())
        self.assertEqual(source.read_bytes(), original)
        self.assertEqual(outside.read_bytes(), b"\xff" + b"x" * 1000)

    def test_self_test_cli_documents_explicit_opt_in_source_check(self) -> None:
        result = self.command("--help")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("--self-test", result.stdout)
        self.assertIn("--check", result.stdout)

    def test_empty_source_root_still_checks_tools_and_directory_limits(self) -> None:
        directory = self.root / "src"
        directory.mkdir()
        compiler = self.compiler_fixture([])
        accepted = self.command("check", "--ghc", str(compiler))
        self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
        self.assertEqual(accepted.stdout, "Source policy: checked 0 source files\n")
        for number in range(21):
            (directory / ("Empty" + str(number))).mkdir()
        rejected = self.command("check", "--ghc", str(compiler))
        self.assertEqual(rejected.returncode, 1, rejected.stdout + rejected.stderr)
        self.assertEqual(rejected.stderr, "src:1:1: directory_entries: actual 21, limit 20\n")

    def test_missing_required_tools_fail_explicitly_even_for_empty_roots(self) -> None:
        (self.root / "src").mkdir()
        cases = [("check", "fourmolu", "0.21.0.0"), ("check", "ghc", "9.10.3"),
                 ("shell", "shellcheck", "0.11.0"), ("hlint", "hlint", "3.10")]
        for command, name, version in cases:
            with self.subTest(tool=name):
                result = self.command(command, environment={name.upper(): str(self.root / "absent tool")})
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stderr, "source-policy: tool: " + name + " " + version
                                 + " is required (executable not found)\n")

    def test_wrong_shellcheck_version_is_rejected(self) -> None:
        executable = self.root / "old shellcheck"
        executable.write_text('#!/bin/sh\nprintf "ShellCheck\\nversion: 0.10.0\\n"\n')
        executable.chmod(0o755)
        result = self.command("shell", environment={"SHELLCHECK": str(executable)})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "source-policy: tool: unsupported shellcheck 0.10.0; required 0.11.0\n")

    def test_engine_facts_are_complete_validated_and_not_silently_coerced(self) -> None:
        path = "src/Example.hs"
        files = {path: "value = 1\n"}
        record = protocol_file(path, 1)
        valid = json.dumps(record)
        failures: list[str] = ["", "not JSON", "{}", valid + "\n" + valid,
                               valid.replace(path, "outside.hs"),
                               valid.replace('"code_lines": [1]', '"code_lines": [true]'),
                               valid.replace('"code_lines": [1]', '"code_lines": [2]'),
                               valid.replace('"code_lines": [1]', '"code_lines": [1, 1]'),
                               valid.replace('"top_level": []', '"top_level": [{"line":1,"column":20,"name":"x"}]'),
                               valid.replace('"definitions": []', '"definitions": [{"line":1,"column":1,"name":"x","lines":2}]'),
                               valid.replace('"strings": []', '"strings": [{"line":1,"column":1,"value":7}]'),
                               valid.replace('"path":', '"path":"ignored", "path":')]
        for output in failures:
            with self.subTest(output=output):
                with self.assertRaises(FactsError):
                    decode_facts(output, files)
        self.assertEqual(set(decode_facts(valid, files)), {path})

    def test_diagnostics_are_identical_across_fact_and_file_enumeration_orders(self) -> None:
        files = {"src/Z.hs": "-- " + "z" * 78 + "\n", "src/A.hs": "-- " + "a" * 79 + "\n"}
        records = [protocol_file(path) for path in files]
        expected = ["src:1:1: directory_entries: actual 21, limit 20",
                    "src/A.hs:1:81: line_width: actual 82, limit 80",
                    "src/Z.hs:1:81: line_width: actual 81, limit 80"]
        for _ in range(2):
            self.assertEqual(report(files, records, {"src": 21}), expected)
            self.assertEqual(report(dict(reversed(list(files.items()))), list(reversed(records)), {"src": 21}), expected)

    def test_external_edit_during_engine_execution_cannot_pass_even_if_bytes_are_restored(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        original = b"module Example where\n\nvalue = 1\n"
        source.write_bytes(original)
        record = protocol_file("src/Example.hs")
        record["code_lines"] = [1, 3]
        compiler = self.compiler_fixture([record], engine_code=(
            "from pathlib import Path\nsource: Path = Path(" + repr(str(source)) + ")\n"
            "source.write_bytes(b'changed')\nsource.write_bytes(" + repr(original) + ")"
        ))
        result = self.command("check", "--ghc", str(compiler))
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "src/Example.hs: input: source tree changed during policy check\n")
        self.assertEqual(source.read_bytes(), original)

    def test_external_edit_during_format_check_prevents_engine_execution(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        source.write_bytes(b"module Example where\n\nvalue = 1\n")
        marker = self.root / "engine was called"
        compiler = self.compiler_fixture([protocol_file("src/Example.hs")], engine_code=(
            "from pathlib import Path\nPath(" + repr(str(marker)) + ").write_text('ran')"
        ))
        formatter = self.root / "external edit formatter fixture"
        formatter.write_text(
            '#!/usr/bin/env python3\nfrom pathlib import Path\nimport sys\n'
            'if "--version" in sys.argv:\n    print("fourmolu 0.21.0.0")\n'
            'else:\n    Path(' + repr(str(source)) + ').write_text("module Example where\\n")\n'
        )
        formatter.chmod(0o755)
        result = self.command("check", "--ghc", str(compiler), environment={"FOURMOLU": str(formatter)})
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "src/Example.hs: input: source tree changed during policy check\n")
        self.assertFalse(marker.exists())

    def test_missing_file_facts_and_engine_failures_cannot_pass(self) -> None:
        source = self.root / "src" / "Example.hs"
        source.parent.mkdir()
        source.write_text("module Example where\n\nvalue = 1\n")
        compiler = self.compiler_fixture([])
        result = self.command("check", "--ghc", str(compiler))
        self.assertEqual(result.returncode, 1)
        self.assertIn("source-policy: facts:", result.stderr)
        compiler = self.compiler_fixture([], engine_code="import sys\nsys.stderr.write('src/Example.hs:3:1: parser failure\\n')\nsys.exit(1)")
        result = self.command("check", "--ghc", str(compiler))
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, "source-policy: tool: GHC facts engine failed\nsrc/Example.hs:3:1: parser failure\n")


class WorkingCheckoutScripts(unittest.TestCase):
    """Invoke copied public scripts without overriding their checkout root."""

    directory: Path

    def setUp(self) -> None:
        temporary = tempfile.TemporaryDirectory(prefix="working checkout ")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name)

    def checkout(self, name: str, scripts: str = "scripts") -> Path:
        root = self.directory / name
        root.mkdir(exist_ok=True)
        for directory in ("scripts", ".githooks"):
            shutil.copytree(ROOT / directory, root / directory, dirs_exist_ok=True)
        (root / "test").mkdir(exist_ok=True)
        for pattern in ("test/*.py", "test/SourcePolicy.hs", "fourmolu.yaml", ".hlint.yaml", ".shellcheckrc"):
            for source in ROOT.glob(pattern):
                shutil.copy2(source, root / source.relative_to(ROOT))
        for directory in ("src", "app", "test/reference"):
            (root / directory).mkdir(exist_ok=True)
        if scripts != "scripts":
            (root / "scripts").rename(root / scripts)
            # Configuration remains at its public, fixed location.
            (root / "scripts").mkdir()
            for name in ("source-policy.json", "stack-compiler.yaml"):
                shutil.copy2(ROOT / "scripts" / name, root / "scripts" / name)
        return root

    def invoke(
        self, root: Path, script: str, *arguments: str, scripts: str = "scripts",
        relative: bool = False, environment: Mapping[str, str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        path = Path(scripts) / script
        env = dict(os.environ, **(environment or {}))
        selected = os.environ.get("SOURCE_POLICY_TEST_GHC")
        if selected is not None:
            env["GHC"] = selected
        return subprocess.run(
            ["/bin/sh", str(path if relative else root / path), *arguments],
            cwd=root if relative else self.directory, env=env,
            capture_output=True, text=True, check=False, timeout=120,
        )

    def test_source_script_checks_newline_checkout_not_passing_sibling(self) -> None:
        sibling = self.checkout("checkout")
        good = b"module Example where\n\nvalue :: Int\nvalue = 1\n"
        (sibling / "src/Example.hs").write_bytes(good)
        for suffix, scripts, relative in (("\n", "scripts", False), ("\n\n", "scripts\n", True), ("\n\n\n", "\n", True)):
            with self.subTest(suffix=repr(suffix), scripts=repr(scripts), relative=relative):
                root = self.checkout("checkout" + suffix, scripts)
                bad = b"module Example where\nvalue=1\n"
                (root / "src/Example.hs").write_bytes(bad)
                accepted = self.invoke(sibling, "check-source.sh")
                self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
                result = self.invoke(root, "check-source.sh", scripts=scripts, relative=relative)
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
                self.assertIn("src/Example.hs: formatting:", result.stderr)
                self.assertEqual((root / "src/Example.hs").read_bytes(), bad)
                self.assertEqual((sibling / "src/Example.hs").read_bytes(), good)

    def test_shell_script_checks_newline_checkout_not_passing_sibling(self) -> None:
        sibling = self.checkout("checkout")
        good = b'#!/bin/sh\nargument="two words"\nprintf "%s\\n" "$argument"\n'
        (sibling / "probe.sh").write_bytes(good)
        for suffix, scripts, relative in (("\n", "scripts", False), ("\n\n", "scripts\n", True), ("\n\n\n", "\n", True)):
            with self.subTest(suffix=repr(suffix), scripts=repr(scripts), relative=relative):
                root = self.checkout("checkout" + suffix, scripts)
                bad = b'#!/bin/sh\nargument="two words"\nprintf "%s\\n" $argument\n'
                (root / "probe.sh").write_bytes(bad)
                accepted = self.invoke(sibling, "check-shell.sh")
                self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
                result = self.invoke(root, "check-shell.sh", scripts=scripts, relative=relative)
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
                self.assertIn("probe.sh:3:", result.stderr)
                self.assertIn("SC2086", result.stderr)
                self.assertEqual((root / "probe.sh").read_bytes(), bad)
                self.assertEqual((sibling / "probe.sh").read_bytes(), good)

    def test_hlint_script_checks_newline_checkout_not_passing_sibling(self) -> None:
        sibling = self.checkout("checkout")
        good = b"module Example where\n\nvalue :: Int\nvalue = 1\n"
        (sibling / "src/Example.hs").write_bytes(good)
        for suffix, scripts, relative in (("\n", "scripts", False), ("\n\n", "scripts\n", True), ("\n\n\n", "\n", True)):
            with self.subTest(suffix=repr(suffix), scripts=repr(scripts), relative=relative):
                root = self.checkout("checkout" + suffix, scripts)
                bad = b"module Example where\n\nvalue :: Int\nvalue = id 1\n"
                (root / "src/Example.hs").write_bytes(bad)
                accepted = self.invoke(sibling, "check-hlint.sh")
                self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
                result = self.invoke(root, "check-hlint.sh", scripts=scripts, relative=relative)
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
                self.assertIn("src/Example.hs:4:", result.stderr)
                self.assertIn("Redundant id", result.stderr)
                self.assertEqual((root / "src/Example.hs").read_bytes(), bad)
                self.assertEqual((sibling / "src/Example.hs").read_bytes(), good)

    def test_formatter_writes_newline_checkout_and_preserves_sibling(self) -> None:
        sibling = self.checkout("checkout")
        sibling_original = b"module Example where\nvalue=2\n"
        for suffix, scripts, relative in (("\n", "scripts", False), ("\n\n", "scripts\n", True), ("\n\n\n", "\n", True)):
            with self.subTest(suffix=repr(suffix), scripts=repr(scripts), relative=relative):
                root = self.checkout("checkout" + suffix, scripts)
                (root / "src/Example.hs").write_bytes(b"module Example where\nvalue=1\n")
                untouched = b"module Other where\nother=3\n"
                (root / "src/Other.hs").write_bytes(untouched)
                (sibling / "src/Example.hs").write_bytes(sibling_original)
                result = self.invoke(root, "format-source.sh", "src/Example.hs", scripts=scripts, relative=relative)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual((sibling / "src/Example.hs").read_bytes(), sibling_original)
                self.assertEqual((root / "src/Example.hs").read_bytes(), b"module Example where\n\nvalue = 1\n")
                self.assertEqual((root / "src/Other.hs").read_bytes(), untouched)

    def test_validate_discovers_compiler_using_newline_checkout_configuration(self) -> None:
        selection_environment = dict(os.environ)
        selected_compiler = os.environ.get("SOURCE_POLICY_TEST_GHC")
        if selected_compiler is not None:
            selection_environment["GHC"] = selected_compiler
        selected = subprocess.run(
            ["/bin/sh", str(ROOT / "scripts/validate.sh"), "--print-compiler"],
            env=selection_environment, capture_output=True, text=True, check=False, timeout=120,
        )
        self.assertEqual(selected.returncode, 0, selected.stdout + selected.stderr)
        compiler = selected.stdout.removesuffix("\n")
        tools = self.directory / "discovery tools"
        tools.mkdir()
        for name in ("dirname", "basename", "readlink"):
            executable = shutil.which(name, path=os.defpath)
            self.assertIsNotNone(executable, name)
            assert executable is not None
            (tools / name).symlink_to(executable)
        log = self.directory / "stack arguments.json"
        stack = tools / "stack"
        stack_fixture = tools / "stack fixture.py"
        stack_fixture.write_text(
            "import json\nfrom pathlib import Path\nimport sys\n"
            "log: Path = Path(" + repr(str(log)) + ")\n"
            "log.write_text(json.dumps(sys.argv[1:]))\nprint(" + repr(compiler) + ")\n"
        )
        stack.write_text(
            "#!/bin/sh\nexec " + shlex.quote(sys.executable) + " "
            + shlex.quote(str(stack_fixture)) + ' "$@"\n'
        )
        stack.chmod(0o755)
        environment = dict(os.environ, PATH=str(tools))
        environment.pop("GHC", None)
        self.checkout("checkout")
        for suffix, scripts, relative in (("\n", "scripts", False), ("\n\n", "scripts\n", True), ("\n\n\n", "\n", True)):
            with self.subTest(suffix=repr(suffix), scripts=repr(scripts), relative=relative):
                root = self.checkout("checkout" + suffix, scripts)
                path = Path(scripts) / "validate.sh"
                result = subprocess.run(
                    ["/bin/sh", str(path if relative else root / path), "--print-compiler"],
                    cwd=root if relative else self.directory, env=environment,
                    capture_output=True, text=True, check=False, timeout=120,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(result.stdout, compiler + "\n")
                self.assertEqual(result.stderr, "")
                # A relative invocation starts from subprocess's physical cwd.
                # Resolve only its parent; keep the newline-bearing name exact.
                expected_root: Path = root.parent.resolve() / root.name if relative else root
                self.assertEqual(json.loads(log.read_text()), [
                    "--stack-yaml", str(expected_root / "scripts/stack-compiler.yaml"),
                    "--no-install-ghc", "path", "--compiler-exe",
                ])


class RealSourcePolicyBoundaries(unittest.TestCase):
    """The six exact boundaries through real formatting, GHC facts, and scripts."""

    compiler: str
    root: Path

    @classmethod
    def setUpClass(cls) -> None:
        selected = os.environ.get("SOURCE_POLICY_TEST_GHC")
        if selected is None:
            raise unittest.SkipTest("pass --ghc PATH to run real-engine checker boundaries")
        cls.compiler = selected

    def setUp(self) -> None:
        temporary = tempfile.TemporaryDirectory(prefix="source-policy-boundary-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        (self.root / "src").mkdir()

    def script(self, name: str, *arguments: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["sh", str(ROOT / "scripts" / name), *arguments, "--root", str(self.root), "--ghc", self.compiler],
            env=dict(os.environ), capture_output=True, text=True, check=False, timeout=120,
        )

    def specimen(self, source: str) -> str:
        path = self.root / "src" / "Example.hs"
        path.write_bytes(source.encode("utf-8"))
        formatted = self.script("format-source.sh")
        self.assertEqual(formatted.returncode, 0, formatted.stdout + formatted.stderr)
        return path.read_bytes().decode("utf-8")

    def assert_checked(self, expected: str | None) -> None:
        source = self.root / "src" / "Example.hs"
        original = source.read_bytes()
        first = self.script("check-source.sh")
        if expected is None:
            self.assertEqual(first.returncode, 0, first.stdout + first.stderr)
            self.assertEqual(first.stdout, "Source policy: checked 1 source files\n")
            self.assertEqual(first.stderr, "")
        else:
            self.assertEqual(first.returncode, 1, first.stdout + first.stderr)
            self.assertEqual(first.stderr, expected + "\n")
            self.assertEqual(first.stdout, "")
        self.assertEqual(source.read_bytes(), original)

    def test_file_lines_500_pass_and_501_fail_without_another_violation(self) -> None:
        for code_count in (500, 501):
            with self.subTest(code_lines=code_count):
                functions = [sized_function("value" + str(number), 47) for number in range(9)]
                functions.append(sized_function("value9", 46 if code_count == 500 else 47))
                formatted = self.specimen("module Example where\n\n" + "\n\n".join(functions) + "\n")
                expected = None if code_count == 500 else (
                    "src/Example.hs:" + str(len(formatted.splitlines())) + ":1: file_lines: actual 501, limit 500"
                )
                self.assert_checked(expected)

    def test_top_level_20_pass_and_21_fail(self) -> None:
        for count in (20, 21):
            with self.subTest(top_level=count):
                self.specimen("module Example where\n\n" + "\n\n".join(
                    "value" + str(number) + " = " + str(number) for number in range(count)) + "\n")
                self.assert_checked(None if count == 20 else "src/Example.hs:43:1: top_level: actual 21, limit 20")

    def test_definition_50_pass_and_51_fail_excluding_separate_signature(self) -> None:
        for count in (50, 51):
            with self.subTest(definition_lines=count):
                self.specimen("module Example where\n\n" + sized_function("value", count - 2) + "\n")
                self.assert_checked(None if count == 50 else "src/Example.hs:4:1: definition_lines: actual 51, limit 50")

    def test_physical_comment_width_80_pass_and_81_fail(self) -> None:
        for width in (80, 81):
            with self.subTest(width=width):
                self.specimen("module Example where\n\n-- " + "é" * (width - 3) + "\n")
                self.assert_checked(None if width == 80 else "src/Example.hs:3:81: line_width: actual 81, limit 80")

    def test_decoded_literal_occurrences_2_pass_and_3_fail(self) -> None:
        for count in (2, 3):
            with self.subTest(string_occurrences=count):
                declarations = ['a = "a"', 'b = "\\97"', 'c = "a"'][:count]
                self.specimen("module Example where\n\n" + "\n\n".join(declarations) + "\n")
                self.assert_checked(None if count == 2 else "src/Example.hs:7:5: string_occurrences: actual 3, limit 2")

    def test_immediate_directory_entries_20_pass_and_21_fail_with_real_file_facts(self) -> None:
        self.specimen("module Example where\n\nvalue = 1\n")
        for number in range(19):
            (self.root / "src" / (".hidden" + str(number))).mkdir()
        self.assert_checked(None)
        (self.root / "src" / "Nested").mkdir()
        self.assert_checked("src:1:1: directory_entries: actual 21, limit 20")

    def test_new_hidden_nested_sources_report_deterministically_and_exempt_other_roots(self) -> None:
        sources: dict[str, bytes] = {
            "src/Z.hs": ("module Z where\n\n-- " + "z" * 79 + "\n").encode("utf-8"),
            "src/.hidden/A.hs": ("module A where\n\n-- " + "a" * 78 + "\n").encode("utf-8"),
        }
        outside = self.root / "test" / "NotScoped.hs"
        outside.parent.mkdir()
        outside.write_bytes(b"\xff" + b"x" * 1000)
        expected = ("src/.hidden/A.hs:3:81: line_width: actual 81, limit 80\n"
                    "src/Z.hs:3:81: line_width: actual 82, limit 80\n")
        for order in (list(sources), list(reversed(sources))):
            for name in sources:
                (self.root / name).unlink(missing_ok=True)
            for name in order:
                path = self.root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(sources[name])
            formatted = self.script("format-source.sh")
            self.assertEqual(formatted.returncode, 0, formatted.stdout + formatted.stderr)
            rejected = self.script("check-source.sh")
            self.assertEqual(rejected.returncode, 1)
            self.assertEqual(rejected.stderr, expected)
            self.assertEqual(rejected.stdout, "")
            for name, content in sources.items():
                self.assertEqual((self.root / name).read_bytes(), content)
        self.assertEqual(outside.read_bytes(), b"\xff" + b"x" * 1000)


def main() -> int:
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--ghc")
    arguments, remaining = parser.parse_known_args(namespace=TestArguments())
    if arguments.ghc is not None:
        os.environ["SOURCE_POLICY_TEST_GHC"] = arguments.ghc
    program = unittest.main(argv=[sys.argv[0], *remaining], exit=False)
    return 0 if program.result.wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
