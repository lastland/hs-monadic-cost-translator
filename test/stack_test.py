#!/usr/bin/env python3
"""Stack test wrapper regressions; no installed Haskell tools are required."""

import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "stack-test.sh"


class StackTestWrapperTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="stack test wrapper ")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.tools = self.directory / "fake tools"
        self.tools.mkdir()
        self.cwd = self.directory / "outside checkout"
        self.cwd.mkdir()
        self.logs = self.directory / "retained logs"
        self.logs.mkdir()
        self.env = {
            "PATH": str(self.tools),
            "HOME": str(self.directory),
            "TMPDIR": str(self.logs),
            "LC_ALL": "C",
        }
        # Expose shell utilities, never an installed Stack or Haskell toolchain.
        for name in ("mktemp", "tail"):
            executable = shutil.which(name, path=os.defpath)
            self.assertIsNotNone(executable, name)
            (self.tools / name).symlink_to(executable)

    @staticmethod
    def write_tool(path, body):
        path.write_text("#!/bin/sh\nset -eu\n" + body)
        path.chmod(0o755)
        return path

    def run_script(self, *arguments):
        return subprocess.run(
            ["/bin/sh", str(SCRIPT), *arguments],
            cwd=self.cwd,
            env=self.env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=5,
        )

    def stack(self, stdout=b"", stderr=b"", status=0):
        return self.write_tool(
            self.tools / "stack",
            '[ "$1" = test ] || exit 99\n'
            + "printf '%s' " + shlex.quote(stdout.decode("ascii")) + "\n"
            + "printf '%s' " + shlex.quote(stderr.decode("ascii")) + " >&2\n"
            + "exit " + str(status) + "\n",
        )

    def log_path(self, result):
        match = re.match(rb"[^\r\n]+\nLog: ([^\r\n]+)\n", result.stdout)
        self.assertIsNotNone(match, result.stdout)
        path = self.cwd / os.fsdecode(match.group(1))
        self.assertEqual(path.parent, self.logs)
        return path

    def failure_excerpt(self, result, status):
        self.assertEqual(result.returncode, status, result.stdout + result.stderr)
        self.assertEqual(result.stderr, b"")
        self.assertEqual(
            result.stdout.split(b"\n", 1)[0],
            ("FAIL (exit " + str(status) + ")").encode("ascii"),
        )
        self.log_path(result)
        return result.stdout.split(b"\n", 2)[2]

    def test_successful_verbose_child_retains_complete_log_without_echoing_it(self):
        stdout = (b"child stdout: " + b"o" * 200 + b"\n") * 100
        stdout += b"stdout without final newline"
        stderr = (b"child stderr: " + b"e" * 200 + b"\n") * 100
        stderr += b"stderr without final newline"
        self.stack(stdout, stderr)

        result = self.run_script()

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stderr, b"")
        lines = result.stdout.splitlines()
        self.assertEqual(len(lines), 2, result.stdout)
        self.assertRegex(lines[0], rb"^PASS(?:[ :][^\r\n]*)?$")
        self.assertLessEqual(len(lines[0]), 80)
        log = self.log_path(result)
        self.assertTrue(log.is_file(), str(log))
        self.assertEqual(log.read_bytes(), stdout + stderr)

    def test_nonzero_statuses_preserve_complete_merged_log(self):
        stdout = b"stdout before failure\n"
        stderr = b"stderr explains failure\n"
        for status in (1, 23, 137, 255):
            with self.subTest(status=status):
                self.stack(stdout, stderr, status)

                result = self.run_script()

                self.failure_excerpt(result, status)
                self.assertEqual(self.log_path(result).read_bytes(), stdout + stderr)

    def test_failure_excerpt_is_exactly_the_final_60_lines(self):
        lines = [("line %03d\n" % number).encode("ascii") for number in range(100)]
        lines[90] = b"Log: this is child output, not the wrapper log path\n"
        self.stack(b"".join(lines[:50]), b"".join(lines[50:]), status=41)

        result = self.run_script()

        self.assertEqual(self.failure_excerpt(result, 41), b"".join(lines[40:]))
        self.assertEqual(self.log_path(result).read_bytes(), b"".join(lines))

    def test_failure_excerpt_caps_bytes_even_for_enormous_lines(self):
        payloads = {
            "single unterminated line": b"start: " + b"x" * 20000 + b" final bytes",
            "many long lines": b"".join(
                ("line %03d " % number).encode("ascii") + b"x" * 1000 + b"\n"
                for number in range(100)
            ),
        }
        for name, payload in payloads.items():
            with self.subTest(payload=name):
                self.stack(stderr=payload, status=42)

                result = self.run_script()

                excerpt = self.failure_excerpt(result, 42)
                self.assertLessEqual(len(excerpt), 12000)
                self.assertLessEqual(len(excerpt.splitlines()), 60)
                self.assertEqual(excerpt, payload[-12000:])
                self.assertEqual(self.log_path(result).read_bytes(), payload)

    def test_exact_arguments_and_caller_working_directory_are_preserved(self):
        calls = self.directory / "stack call record"
        self.write_tool(
            self.tools / "stack",
            "printf '%s\\000' \"$PWD\" \"$@\" > " + shlex.quote(str(calls)) + "\n",
        )
        arguments = (
            "--test-arguments",
            "with spaces",
            "",
            "*?[glob]",
            "$(not-run); & | < > `not-run`",
            "quotes '\" and \\",
            "line one\nline two",
            "--",
        )

        result = self.run_script(*arguments)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stderr, b"")
        self.assertEqual(
            calls.read_bytes().split(b"\0"),
            [os.fsencode(self.cwd), b"test", *map(os.fsencode, arguments), b""],
        )
        self.assertEqual(self.log_path(result).read_bytes(), b"")

    def test_repeated_calls_keep_unique_complete_logs(self):
        payloads = (b"first retained output\n", b"second retained output\n")
        logs = []
        for payload in payloads:
            self.stack(stdout=payload)

            result = self.run_script()

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            logs.append(self.log_path(result))

        self.assertNotEqual(logs[0], logs[1])
        for log, payload in zip(logs, payloads):
            self.assertEqual(log.read_bytes(), payload)

    def test_failed_mktemp_prevents_stack_invocation(self):
        called = self.directory / "stack was called"
        self.write_tool(
            self.tools / "stack",
            "printf invoked > " + shlex.quote(str(called)) + "\n",
        )
        (self.tools / "mktemp").unlink()
        self.write_tool(
            self.tools / "mktemp",
            "printf 'fixture log creation failed\\n' >&2\nexit 73\n",
        )

        result = self.run_script()

        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, b"")
        self.assertIn(b"fixture log creation failed", result.stderr)
        self.assertFalse(called.exists())
        self.assertEqual(list(self.logs.iterdir()), [])

    def test_missing_stack_is_a_logged_failure(self):
        result = self.run_script()

        excerpt = self.failure_excerpt(result, 127)
        log = self.log_path(result).read_bytes()
        self.assertIn(b"stack", log)
        self.assertEqual(excerpt, log)

    def test_excerpt_tool_failure_does_not_replace_stack_status(self):
        stdout = b"output remains available in the retained log\n"
        self.stack(stdout=stdout, status=37)
        called = self.directory / "tail was called"
        (self.tools / "tail").unlink()
        self.write_tool(
            self.tools / "tail",
            "printf invoked > " + shlex.quote(str(called)) + "\n"
            + "printf 'fixture tail failed\\n' >&2\nexit 91\n",
        )

        result = self.run_script()

        self.assertEqual(self.failure_excerpt(result, 37), b"")
        self.assertTrue(called.is_file())
        self.assertEqual(self.log_path(result).read_bytes(), stdout)


if __name__ == "__main__":
    unittest.main()
