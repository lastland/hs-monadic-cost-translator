#!/usr/bin/env python3
"""Compiler-selection regressions; no installed Haskell tools are required."""

import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "validate.sh"
PIN = "9.10.3"
OTHER = "9.8.4"


class CompilerPinTests(unittest.TestCase):
    def test_script_and_cabal_pin_the_supported_compiler(self):
        self.assertEqual(
            re.findall(r"(?m)^required_ghc=([^\s#]+)[ \t]*$", SCRIPT.read_text()),
            [PIN],
        )
        self.assertEqual(
            re.sub(
                r"(?m)^[ \t]*#[^\n]*\n", "",
                (ROOT / "scripts" / "stack-compiler.yaml").read_text(),
            ),
            "resolver: ghc-" + PIN + "\npackages: []\n",
        )
        cabal = re.sub(r"(?m)--[^\n]*", "", (ROOT / "cost-translate.cabal").read_text())
        tested = re.findall(r"(?m)^tested-with:([^\n]*(?:\n[ \t]+[^\n]*)*)", cabal)
        self.assertEqual(["".join(value.split()) for value in tested], ["GHC==" + PIN])
        # Check only these stanzas' direct compiler dependencies, not a Cabal parser.
        for heading, required in (
            ("library", {"ghc", "ghc-boot-th"}),
            ("test-suite cost-translate-test", {"ghc"}),
        ):
            with self.subTest(stanza=heading):
                stanza = re.search(
                    r"(?ms)^" + re.escape(heading) + r"[ \t]*\n(.*?)(?=^\S|\Z)",
                    cabal,
                )
                self.assertIsNotNone(stanza)
                bounds = re.findall(
                    r"(?m)^[ \t]+(ghc(?:-boot-th)?)(?=[ \t,]|$)([^\n]*)",
                    stanza.group(1),
                )
                self.assertTrue(required <= {name for name, _ in bounds})
                self.assertEqual(len(bounds), len(dict(bounds)), "duplicate compiler dependency")
                for name, bound in bounds:
                    self.assertEqual("".join(bound.rstrip().rstrip(",").split()), "==" + PIN, name)


class CompilerPreflightTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="compiler preflight ")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.tools = self.directory / "tools"
        self.tools.mkdir()
        self.cwd = self.directory / "outside checkout"
        self.cwd.mkdir()
        self.log = self.directory / "calls"
        self.log.touch()
        self.translator = self.directory / "translator output" / "cost-translate"
        self.write_tool(self.translator, "exit 0\n")
        self.env = {
            "PATH": str(self.tools),
            "HOME": str(self.directory),
            "LC_ALL": "C",
            "PREFLIGHT_LOG": str(self.log),
            "PREFLIGHT_TRANSLATOR": str(self.translator),
            "STACK_COMPILER": "",
            "STACK_STATUS": "1",
        }
        # Only shell utilities needed by validate.sh escape the fake toolchain.
        for name in ("dirname", "basename", "readlink"):
            executable = shutil.which(name, path=os.defpath)
            self.assertIsNotNone(executable, name)
            (self.tools / name).symlink_to(executable)
        self.write_tool(
            self.tools / "cabal",
            'ghc=$(command -v ghc || :)\n'
            'pkg=$(command -v ghc-pkg || :)\n'
            'translator=$(command -v cost-translate || :)\n'
            '{ printf "%s\\t" cabal "$PWD" "$ghc" "$pkg" "$translator" "$@"; '
            'printf "\\n"; } >> "$PREFLIGHT_LOG"\n'
            '[ "${CABAL_FAIL-}" != "$1" ] || exit 23\n'
            'if [ "$1" = list-bin ]; then printf "%s\\n" "$PREFLIGHT_TRANSLATOR"; fi\n',
        )
        self.write_tool(
            self.tools / "stack",
            self.record("stack")
            + 'printf "%s\\n" "$STACK_COMPILER"\nexit "$STACK_STATUS"\n',
        )
        self.write_tool(
            self.tools / "python3",
            self.record("python3") + 'exit "${POLICY_STATUS-0}"\n',
        )

    @staticmethod
    def record(name):
        # Tab-separated records preserve argument boundaries, including spaces.
        return (
            '{ printf "%s\\t" ' + shlex.quote(name) + ' "$0" "$@"; '
            'printf "\\n"; } >> "$PREFLIGHT_LOG"\n'
        )

    @staticmethod
    def write_tool(path, body):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("#!/bin/sh\nset -eu\n" + body)
        path.chmod(0o755)
        return path

    def compiler(self, path, version=PIN, status=0):
        return self.write_tool(
            path,
            self.record("ghc")
            + '[ "$#" -eq 1 ] && [ "$1" = --numeric-version ] || exit 99\n'
            + "printf '%s\\n' " + shlex.quote(version) + "\nexit " + str(status) + "\n",
        )

    def installation(self, name="installed compiler", version=PIN):
        directory = self.directory / name
        compiler = self.compiler(directory / "ghc", version)
        self.package_tool(directory / "ghc-pkg", version)
        return compiler

    def package_tool(self, path, version=PIN):
        return self.write_tool(
            path,
            self.record("ghc-pkg")
            + '[ "$#" -eq 1 ] && [ "$1" = --version ] || exit 99\n'
            + "printf '%s\\n' " + shlex.quote("GHC package manager version " + version) + "\n",
        )

    def expose(self, name, compiler):
        (self.tools / name).symlink_to(compiler)

    def stack_compiler(self, compiler):
        self.env.update(STACK_COMPILER=str(compiler), STACK_STATUS="0")

    def run_script(self, *arguments):
        return subprocess.run(
            ["/bin/sh", str(SCRIPT), *arguments],
            cwd=self.cwd,
            env=self.env,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=5,
        )

    def calls(self, name=None):
        calls = [line.split("\t")[:-1] for line in self.log.read_text().splitlines()]
        return calls if name is None else [call[1:] for call in calls if call[0] == name]

    def assert_rejected(self, result, diagnostic):
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(diagnostic, result.stderr)
        self.assertEqual(self.calls("cabal"), [])
        self.assertEqual(self.calls("python3"), [])

    def assert_selected(self, result, compiler, build=True):
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("Validating with GHC " + PIN + " (" + str(compiler) + ")", result.stdout)
        calls = self.calls("cabal")
        if not build:
            self.assertEqual(calls, [])
            self.assertEqual(self.calls("python3"), [])
            return
        self.assertEqual(
            self.calls("python3"),
            [[str(self.tools / "python3"), str(ROOT / "test" / "comment_policy.py"),
              "--ghc", str(compiler), "--self-test", "--check"]],
        )
        self.assertEqual([call[4] for call in calls], ["build", "list-bin", "test"])
        flags = [
            "--with-compiler=" + str(compiler),
            "--with-hc-pkg=" + str(compiler.parent / "ghc-pkg"),
            "--ghc-options=-Wall -Werror",
        ]
        self.assertEqual(calls[0][4:7], ["build", "exe:cost-translate", "test:cost-translate-test"])
        self.assertEqual(calls[1][4:6], ["list-bin", "exe:cost-translate"])
        self.assertEqual(calls[2][4:6], ["test", "cost-translate-test"])
        for index, call in enumerate(calls):
            with self.subTest(cabal=call[4]):
                cwd, ghc, package, translator, *arguments = call
                self.assertEqual(cwd, str(ROOT))
                self.assertEqual(ghc, str(compiler.parent / "ghc"))
                self.assertEqual(package, str(compiler.parent / "ghc-pkg"))
                self.assertEqual(translator, str(self.translator) if index == 2 else "")
                self.assertEqual(arguments[-3:], flags)
                for flag in flags:
                    self.assertEqual(arguments.count(flag), 1)

    def test_explicit_compiler_paths_and_command_are_resolved(self):
        compiler = self.installation("installation with spaces/bin")
        alias = self.directory / "compiler override with spaces"
        alias.symlink_to(compiler)
        self.expose("chosen-ghc", compiler)
        for override in (str(alias), os.path.relpath(alias, self.cwd), "chosen-ghc"):
            with self.subTest(override=override):
                self.log.write_text("")
                self.env["GHC"] = override
                build = override == str(alias)
                arguments = () if build else ("--check-compiler",)
                self.assert_selected(self.run_script(*arguments), compiler, build=build)
                self.assertEqual(self.calls("stack"), [])

    def test_discovery_prefers_versioned_compiler(self):
        compiler = self.installation("versioned installation")
        self.expose("ghc-" + PIN, compiler)
        self.expose("ghc", self.installation("default installation"))
        self.stack_compiler(self.installation("stack installation"))
        self.assert_selected(self.run_script("--check-compiler"), compiler, build=False)
        self.assertNotIn(str(self.tools / "ghc"), [call[0] for call in self.calls("ghc")])
        self.assertEqual(self.calls("stack"), [])

    def test_discovery_tries_matching_ghc_after_versioned_candidate(self):
        compiler = self.installation()
        self.expose("ghc", compiler)
        for status in (0, 1):
            with self.subTest(version_query_status=status):
                self.log.write_text("")
                self.compiler(self.tools / ("ghc-" + PIN), OTHER, status)
                self.assert_selected(self.run_script("--check-compiler"), compiler, build=False)
                self.assertEqual(
                    [call[0] for call in self.calls("ghc")[:2]],
                    [str(self.tools / ("ghc-" + PIN)), str(self.tools / "ghc")],
                )
                self.assertEqual(self.calls("stack"), [])

    def test_stack_discovery_is_last_and_never_installs(self):
        self.compiler(self.tools / ("ghc-" + PIN), OTHER)
        self.compiler(self.tools / "ghc", OTHER)
        compiler = self.installation("stack installation")
        self.stack_compiler(compiler)
        self.assert_selected(self.run_script(), compiler)
        self.assertEqual(
            [call[0] for call in self.calls()[:3]], ["ghc", "ghc", "stack"]
        )
        self.assertEqual(
            self.calls("stack"),
            [[str(self.tools / "stack"), "--stack-yaml", str(ROOT / "scripts" / "stack-compiler.yaml"),
              "--no-install-ghc", "path", "--compiler-exe"]],
        )

    def test_explicit_unsupported_compiler_never_falls_back(self):
        self.env["GHC"] = str(self.installation("unsupported", OTHER))
        self.expose("ghc-" + PIN, self.installation("supported"))
        self.stack_compiler(self.installation("stack installation"))
        self.assert_rejected(self.run_script(), "unsupported GHC " + OTHER)
        self.assertEqual(self.calls("stack"), [])
        self.assertEqual([call[0] for call in self.calls("ghc")], [self.env["GHC"]])

    def test_unsupported_stack_result_is_rejected(self):
        self.stack_compiler(self.installation(version=OTHER))
        self.assert_rejected(self.run_script(), "unsupported GHC " + OTHER)
        self.assertEqual(len(self.calls("stack")), 1)

    def test_missing_compiler_with_or_without_stack(self):
        for has_stack in (True, False):
            with self.subTest(stack=has_stack):
                self.log.write_text("")
                if not has_stack:
                    (self.tools / "stack").unlink()
                self.assert_rejected(self.run_script(), "GHC " + PIN + " is required")
                self.assertEqual(len(self.calls("stack")), int(has_stack))

    def test_package_tool_must_exist_and_match(self):
        compiler = self.installation()
        self.env["GHC"] = str(compiler)
        for version in (OTHER, None):
            with self.subTest(package_version=version):
                self.log.write_text("")
                package = compiler.parent / "ghc-pkg"
                if version is None:
                    package.unlink()
                else:
                    self.package_tool(package, version)
                self.assert_rejected(self.run_script(), "must provide ghc-pkg " + PIN)
                self.assertEqual(self.calls("stack"), [])

    def test_selected_versioned_compiler_requires_matching_sibling_ghc(self):
        compiler = self.installation()
        selected = self.compiler(compiler.parent / ("ghc-" + PIN))
        self.env["GHC"] = str(selected)
        for version in (OTHER, None):
            with self.subTest(sibling_version=version):
                self.log.write_text("")
                if version is None:
                    compiler.unlink()
                else:
                    self.compiler(compiler, version)
                self.assert_rejected(self.run_script(), "must provide ghc " + PIN)
                self.assertEqual(self.calls("stack"), [])

    def test_missing_or_unexecutable_override_never_falls_back(self):
        self.expose("ghc-" + PIN, self.installation())
        blocked = self.compiler(self.directory / "not executable")
        blocked.chmod(0o644)
        for override in (str(self.directory / "missing"), str(blocked), ""):
            with self.subTest(override=override):
                self.log.write_text("")
                self.env["GHC"] = override
                self.assert_rejected(self.run_script(), "executable")
                self.assertEqual(self.calls(), [])

    def test_explicit_compiler_query_failure_never_falls_back(self):
        self.expose("ghc-" + PIN, self.installation())
        compiler = self.compiler(self.directory / "broken compiler", status=1)
        self.env["GHC"] = str(compiler)
        self.assert_rejected(self.run_script(), "cannot query compiler")
        self.assertEqual(self.calls("stack"), [])
        self.assertEqual([call[0] for call in self.calls("ghc")], [str(compiler)])

    def test_manager_symlink_uses_real_installation_not_wrong_default(self):
        compiler = self.installation("real installation/bin")
        self.compiler(self.tools / "ghc", OTHER)
        self.package_tool(self.tools / "ghc-pkg", OTHER)
        link = self.directory / "installation link"
        link.symlink_to(compiler)
        self.expose("ghc-" + PIN, Path("..") / link.name)
        self.assert_selected(self.run_script(), compiler)
        self.assertNotIn(str(self.tools / "ghc"), [call[0] for call in self.calls("ghc")])
        self.assertEqual(self.calls("stack"), [])

    def test_check_compiler_does_not_require_or_invoke_cabal(self):
        compiler = self.installation()
        self.env["GHC"] = str(compiler)
        (self.tools / "cabal").unlink()
        (self.tools / "python3").unlink()
        self.assert_selected(self.run_script("--check-compiler"), compiler, build=False)
        self.assertEqual(self.calls("stack"), [])

    def test_comment_policy_failure_stops_validation_before_cabal(self):
        self.env["GHC"] = str(self.installation())
        self.env["POLICY_STATUS"] = "19"
        result = self.run_script()
        self.assertEqual(result.returncode, 19, result.stdout + result.stderr)
        self.assertEqual(len(self.calls("python3")), 1)
        self.assertEqual(self.calls("cabal"), [])

    def test_full_validation_requires_python(self):
        self.env["GHC"] = str(self.installation())
        (self.tools / "python3").unlink()
        self.assert_rejected(self.run_script(), "python3 is required for validation")

    def test_cabal_build_and_list_bin_failures_stop_validation(self):
        self.env["GHC"] = str(self.installation())
        for phase, expected in (("build", ["build"]), ("list-bin", ["build", "list-bin"])):
            with self.subTest(failure=phase):
                self.log.write_text("")
                self.env["CABAL_FAIL"] = phase
                result = self.run_script()
                self.assertEqual(result.returncode, 23, result.stdout + result.stderr)
                self.assertEqual([call[4] for call in self.calls("cabal")], expected)

    def test_help_and_invalid_options_do_not_invoke_tools(self):
        for arguments, status in (
            (("--help",), 0),
            (("--unknown",), 1),
            (("--check-compiler", "extra"), 1),
            (("--help", "extra"), 1),
            (("",), 1),
        ):
            with self.subTest(arguments=arguments):
                self.log.write_text("")
                result = self.run_script(*arguments)
                self.assertEqual(result.returncode, status, result.stdout + result.stderr)
                self.assertIn("Usage:", result.stdout + result.stderr)
                self.assertEqual(self.calls(), [])


if __name__ == "__main__":
    unittest.main()
