#!/usr/bin/env python3
"""Exercise the staged-check command and hook in disposable Git repositories."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from typing import Final
import unittest


ROOT: Final[Path] = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class WorkingEntry:
    mode: int
    content: bytes


@dataclass(frozen=True)
class CheckoutState:
    index: bytes
    effective_index: bytes
    entries: dict[str, WorkingEntry]


class Arguments(argparse.Namespace):
    ghc: str | None
    self_test: bool
    tests: list[str]


class StagedPolicyTests(unittest.TestCase):
    ghc: str | None = None
    root: Path
    env: dict[str, str]

    def setUp(self) -> None:
        temporary = tempfile.TemporaryDirectory(prefix="staged policy ")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name) / "checkout with spaces"
        self.root.mkdir()
        self.env = dict(os.environ)
        for name in tuple(self.env):
            if name.startswith("GIT_"):
                del self.env[name]
        self.env.update(GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull)
        self.env.pop("PYTHONDONTWRITEBYTECODE", None)
        if self.ghc:
            self.env["GHC"] = self.ghc
        self.git("init", "--quiet")
        # Each fixture stages maintained infrastructure from this checkout. The
        # commands under test must read that index, never a second worktree.
        for directory in ("scripts", ".githooks"):
            for path in sorted((ROOT / directory).glob("*")):
                if path.is_file():
                    self.copy(path.relative_to(ROOT))
        # Source distributions may normalize executable bits. Install the hook
        # explicitly in this disposable repository before invoking it directly.
        (self.root / ".githooks" / "pre-commit").chmod(0o755)
        for name in ("fourmolu.yaml", ".hlint.yaml", ".shellcheckrc"):
            if (ROOT / name).is_file():
                self.copy(name)
        if (ROOT / "test" / "SourcePolicy.hs").is_file():
            self.copy("test/SourcePolicy.hs")
        for path in sorted((ROOT / "test").glob("source_policy*.py")):
            self.copy(path.relative_to(ROOT))
        for path in sorted((ROOT / "test" / "source-policy-engine").rglob("*.hs")):
            self.copy(path.relative_to(ROOT))
        self.write("src/Good.hs", "module Good where\n\nvalue :: Int\nvalue = 1\n")
        self.write("app/Main.hs", "module Main where\n\nmain :: IO ()\nmain = pure ()\n")
        self.write("test/Smoke.hs", "module Smoke where\n\nsmoke :: Bool\nsmoke = True\n")
        self.write("test/reference/Reference.hs", "module Reference where\n")
        self.git("add", "--all")

    def copy(self, relative: str | Path) -> None:
        destination = self.root / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, destination)
        destination.chmod((ROOT / relative).stat().st_mode & 0o777)

    def write(self, relative: str, content: str) -> Path:
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        return path

    def git(self, *arguments: str, input: bytes | None = None) -> bytes:
        return subprocess.run(
            ["git", *arguments], cwd=self.root, env=self.env,
            input=input, capture_output=True, check=True,
        ).stdout

    def state(self) -> CheckoutState:
        entries: dict[str, WorkingEntry] = {}
        for directory, children, names in os.walk(self.root):
            children[:] = sorted(name for name in children if name != ".git")
            for name in sorted(children + names):
                path = Path(directory) / name
                relative = path.relative_to(self.root).as_posix()
                entries[relative] = WorkingEntry(
                    mode=path.lstat().st_mode,
                    content=(os.fsencode(os.readlink(path)) if path.is_symlink()
                             else b"" if path.is_dir() else path.read_bytes()),
                )
        effective = self.root / self.env.get("GIT_INDEX_FILE", ".git/index")
        return CheckoutState(
            (self.root / ".git" / "index").read_bytes(), effective.read_bytes(), entries,
        )

    def run_check(self, hook: bool = False, helper: bool = False) -> subprocess.CompletedProcess[str]:
        before = self.state()
        command: list[str]
        if helper:
            command = [sys.executable, "-I", str(ROOT / "scripts" / "staged_policy.py")]
        elif hook:
            command = [str(self.root / ".githooks" / "pre-commit")]
        else:
            command = ["/bin/sh", str(ROOT / "scripts" / "check-staged.sh")]
        result = subprocess.run(
            command, cwd=self.root, env=self.env, text=True,
            capture_output=True, check=False, timeout=120,
        )
        self.assertEqual(self.state(), before, "staged checks changed the index or working files")
        return result

    def test_staged_symlink_is_rejected_before_any_checks(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("src/linked source.hs", result.stderr)
        self.assertIn("symlink", result.stderr)

    def test_intent_to_add_does_not_substitute_for_staged_content(self) -> None:
        (self.root / "src" / "a unstaged link.hs").symlink_to("Good.hs")
        self.git("add", "--intent-to-add", "--", "src/a unstaged link.hs")
        (self.root / "src" / "z staged link.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/z staged link.hs")
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/z staged link.hs", result.stderr)
        self.assertNotIn("src/a unstaged link.hs", result.stderr)

    def test_missing_staged_check_cannot_use_unstaged_replacement(self) -> None:
        self.git("update-index", "--force-remove", "--", "scripts/check-source.sh")
        self.write("scripts/check-source.sh", "#!/bin/sh\nexit 0\n")
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("missing staged check: scripts/check-source.sh", result.stderr)

    def test_intent_to_add_shell_bootstrap_is_rejected(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        self.git("update-index", "--force-remove", "--", "scripts/check-staged.sh")
        self.git("add", "--intent-to-add", "--", "scripts/check-staged.sh")
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("empty staged bootstrap: scripts/check-staged.sh", result.stderr)

    def test_intent_to_add_python_bootstrap_is_rejected(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        self.git("update-index", "--force-remove", "--", "scripts/staged_policy.py")
        self.git("add", "--intent-to-add", "--", "scripts/staged_policy.py")
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("empty staged bootstrap: scripts/staged_policy.py", result.stderr)

    def test_untracked_import_cannot_bypass_hook_or_write_checkout_bytecode(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        self.write("dataclasses.py", "raise SystemExit(0)\n")
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)
        self.assertFalse((self.root / "__pycache__").exists())

    def test_explicit_pythonpath_shadow_cannot_hide_staged_symlink(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        self.write("dataclasses.py", "raise SystemExit(0)\n")
        self.env["PYTHONPATH"] = str(self.root)
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)
        self.assertFalse((self.root / "__pycache__").exists())

    def test_pythonpath_shadow_cannot_bypass_snapshot_check_children(self) -> None:
        self.write("src/Good.hs", "module Good where\n\nvalue = (\n")
        self.git("add", "--", "src/Good.hs")
        self.write("dataclasses.py", "raise SystemExit(0)\n")
        self.env["PYTHONPATH"] = str(self.root)
        result = self.run_check(hook=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("src/Good.hs", output)
        self.assertIn("formatting", output)
        self.assertFalse((self.root / "__pycache__").exists())

    def test_python_import_overrides_do_not_contaminate_valid_snapshot_checks(self) -> None:
        self.write("dataclasses.py", "raise SystemExit(0)\n")
        self.env.update(PYTHONPATH=str(self.root), PYTHONHOME=str(self.root / "not Python"))
        result = self.run_check(hook=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("Source policy: checked", result.stdout)
        self.assertIn("ShellCheck: checked", result.stdout)
        self.assertIn("HLint: checked", result.stdout)
        self.assertFalse((self.root / "__pycache__").exists())

    def test_replacement_blob_cannot_hide_bad_staged_source_from_hook(self) -> None:
        good = self.git("rev-parse", ":src/Good.hs").strip()
        self.write("src/Good.hs", "module Good where\n\nvalue = (\n")
        self.git("add", "--", "src/Good.hs")
        bad = self.git("rev-parse", ":src/Good.hs").strip()
        self.git("replace", bad.decode(), good.decode())
        result = self.run_check(hook=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("src/Good.hs", output)
        self.assertIn("formatting", output)

    def sibling_root_with_staged_symlink(self) -> None:
        sibling = self.root.with_name("checkout")
        self.root.rename(sibling)
        self.root = sibling.with_name(sibling.name + "\n")
        shutil.copytree(sibling, self.root)
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")

    def test_hook_in_trailing_newline_root_cannot_check_passing_sibling(self) -> None:
        self.sibling_root_with_staged_symlink()
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)

    def test_direct_helper_in_trailing_newline_root_cannot_check_passing_sibling(self) -> None:
        self.sibling_root_with_staged_symlink()
        result = self.run_check(helper=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)

    def concurrent_index_change(
        self, trigger: str, *, split: bool = False, alternate: bool = False,
        helper: bool = False, inplace: bool = False,
    ) -> None:
        index = self.root / ".git" / "index"
        if alternate:
            alternative = self.root.parent / "alternative index"
            shutil.copyfile(index, alternative)
            self.env["GIT_INDEX_FILE"] = "../alternative index"
            index = alternative
        link = self.root / "src" / "linked source.hs"
        link.symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        if split:
            self.git("update-index", "--split-index")
        original = index.read_bytes()
        link.unlink()
        link.symlink_to("another target.hs")
        self.git("add", "--", "src/linked source.hs")
        changed = index.read_bytes()
        self.assertNotEqual(original, changed)
        replacement = self.root.parent / "replacement index"
        replacement.write_bytes(changed)
        index.write_bytes(original)
        real_git = shutil.which("git", path=self.env["PATH"])
        assert real_git is not None
        tools = self.root.parent / "instrumented tools"
        tools.mkdir()
        wrapper = tools / "git"
        wrapper.write_text(
            "#!/usr/bin/env python3\n"
            "import os\nfrom pathlib import Path\nimport subprocess\nimport sys\n"
            "descriptors: tuple[int, ...] = (3,) if os.environ.get('GIT_INDEX_FILE') == '/dev/fd/3' else ()\n"
            "result: subprocess.CompletedProcess[bytes] = subprocess.run(\n"
            "    [os.environ['POLICY_REAL_GIT'], *sys.argv[1:]], capture_output=True,\n"
            "    check=False, pass_fds=descriptors)\n"
            "sys.stdout.buffer.write(result.stdout)\nsys.stderr.buffer.write(result.stderr)\n"
            "marker: Path = Path(os.environ['POLICY_MUTATION_MARKER'])\n"
            "if os.environ['POLICY_MUTATION_TRIGGER'] in sys.argv[1:] and not marker.exists():\n"
            "    if os.environ['POLICY_INPLACE'] == '1':\n"
            "        Path(os.environ['POLICY_ORIGINAL_INDEX']).write_bytes(Path(os.environ['POLICY_REPLACEMENT_INDEX']).read_bytes())\n"
            "    else:\n"
            "        Path(os.environ['POLICY_REPLACEMENT_INDEX']).replace(os.environ['POLICY_ORIGINAL_INDEX'])\n"
            "    marker.write_text('atomic replacement performed\\n')\n"
            "raise SystemExit(result.returncode)\n",
            encoding="utf-8",
        )
        wrapper.chmod(0o755)
        marker = self.root.parent / "mutation receipt"
        self.env.update(
            PATH=str(tools) + os.pathsep + self.env["PATH"], POLICY_REAL_GIT=real_git,
            POLICY_MUTATION_TRIGGER=trigger, POLICY_MUTATION_MARKER=str(marker),
            POLICY_ORIGINAL_INDEX=str(index), POLICY_REPLACEMENT_INDEX=str(replacement),
            POLICY_INPLACE="1" if inplace else "0",
        )
        before = self.state()
        command: list[str] = (
            [sys.executable, "-I", str(ROOT / "scripts" / "staged_policy.py")]
            if helper else [str(self.root / ".githooks" / "pre-commit")]
        )
        result = subprocess.run(
            command, cwd=self.root, env=self.env, text=True,
            capture_output=True, check=False, timeout=120,
        )
        self.assertTrue(marker.is_file(), "the real index mutation did not run")
        after = self.state()
        self.assertEqual(after.entries, before.entries)
        self.assertEqual(after.effective_index, changed)
        self.assertEqual(after.index, before.index if alternate else changed)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("index changed during staged checks", result.stderr)

    def test_concurrent_index_update_cannot_turn_a_real_symlink_into_intent_to_add(self) -> None:
        self.concurrent_index_change("--ita-visible-in-index")

    def test_concurrent_split_index_update_is_rejected(self) -> None:
        self.concurrent_index_change("--ita-visible-in-index", split=True)

    def test_concurrent_relative_alternate_index_update_is_rejected(self) -> None:
        self.concurrent_index_change("--ita-visible-in-index", alternate=True)

    def test_concurrent_update_during_shell_bootstrap_is_rejected(self) -> None:
        self.concurrent_index_change(":scripts/check-staged.sh")

    def test_direct_helper_rejects_concurrent_index_update(self) -> None:
        self.concurrent_index_change("--ita-visible-in-index", helper=True)

    def test_in_place_update_during_bootstrap_is_rejected(self) -> None:
        self.concurrent_index_change(":scripts/check-staged.sh", inplace=True)

    def test_concurrent_head_update_cannot_remove_a_real_indexed_symlink(self) -> None:
        self.env.update(
            GIT_AUTHOR_NAME="Policy test", GIT_AUTHOR_EMAIL="test@example.invalid",
            GIT_COMMITTER_NAME="Policy test", GIT_COMMITTER_EMAIL="test@example.invalid",
        )
        base_tree = self.git("write-tree").strip()
        base = self.git("commit-tree", base_tree.decode(), "-m", "before").strip()
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        changed_tree = self.git("write-tree").strip()
        changed = self.git("commit-tree", changed_tree.decode(), "-m", "after").strip()
        self.git("update-ref", "HEAD", base.decode())
        real_git = shutil.which("git", path=self.env["PATH"])
        assert real_git is not None
        tools = self.root.parent / "instrumented tools"
        tools.mkdir()
        wrapper = tools / "git"
        wrapper.write_text(
            "#!/usr/bin/env python3\n"
            "import os\nfrom pathlib import Path\nimport subprocess\nimport sys\n"
            "descriptors: tuple[int, ...] = (3,) if os.environ.get('GIT_INDEX_FILE') == '/dev/fd/3' else ()\n"
            "result: subprocess.CompletedProcess[bytes] = subprocess.run(\n"
            "    [os.environ['POLICY_REAL_GIT'], *sys.argv[1:]], capture_output=True,\n"
            "    check=False, pass_fds=descriptors)\n"
            "sys.stdout.buffer.write(result.stdout)\nsys.stderr.buffer.write(result.stderr)\n"
            "marker: Path = Path(os.environ['POLICY_MUTATION_MARKER'])\n"
            "if '--ita-visible-in-index' in sys.argv[1:] and not marker.exists():\n"
            "    subprocess.run([os.environ['POLICY_REAL_GIT'], 'update-ref', 'HEAD',\n"
            "                    os.environ['POLICY_NEW_HEAD']], check=True, pass_fds=descriptors)\n"
            "    marker.write_text('HEAD atomically updated\\n')\n"
            "raise SystemExit(result.returncode)\n",
            encoding="utf-8",
        )
        wrapper.chmod(0o755)
        marker = self.root.parent / "mutation receipt"
        self.env.update(
            PATH=str(tools) + os.pathsep + self.env["PATH"], POLICY_REAL_GIT=real_git,
            POLICY_MUTATION_MARKER=str(marker), POLICY_NEW_HEAD=changed.decode(),
        )
        result = self.run_check(hook=True)
        self.assertTrue(marker.is_file(), "the real HEAD update did not run")
        self.assertEqual(self.git("rev-parse", "HEAD").strip(), changed)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)

    def alternate_index_is_checked(self, relative: bool) -> None:
        alternative = self.root.parent / "alternative index"
        shutil.copyfile(self.root / ".git" / "index", alternative)
        self.env["GIT_INDEX_FILE"] = "../alternative index" if relative else str(alternative)
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)

    def test_absolute_alternate_index_is_honored_and_preserved(self) -> None:
        self.alternate_index_is_checked(relative=False)

    def test_relative_alternate_index_is_honored_and_preserved(self) -> None:
        self.alternate_index_is_checked(relative=True)

    def test_unchanged_split_index_passes_without_rewriting_index_files(self) -> None:
        self.git("update-index", "--split-index")
        shared: dict[str, bytes] = {
            path.name: path.read_bytes() for path in (self.root / ".git").glob("sharedindex.*")
        }
        self.assertTrue(shared)
        result = self.run_check(hook=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            {path.name: path.read_bytes() for path in (self.root / ".git").glob("sharedindex.*")},
            shared,
        )

    def test_snapshot_temporaries_are_removed_after_success_and_failure(self) -> None:
        temporary = self.root.parent / "temporary snapshots"
        temporary.mkdir()
        self.env["TMPDIR"] = str(temporary)
        accepted = self.run_check(hook=True)
        self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
        self.assertEqual(list(temporary.iterdir()), [])
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        rejected = self.run_check(hook=True)
        self.assertIn("staged symlink is unsupported", rejected.stderr)
        self.assertNotEqual(rejected.returncode, 0, rejected.stdout + rejected.stderr)
        self.assertEqual(list(temporary.iterdir()), [])

    def test_direct_helper_ignores_replacement_blobs(self) -> None:
        good = self.git("rev-parse", ":src/Good.hs").strip()
        self.write("src/Good.hs", "module Good where\n\nvalue = (\n")
        self.git("add", "--", "src/Good.hs")
        bad = self.git("rev-parse", ":src/Good.hs").strip()
        self.git("replace", bad.decode(), good.decode())
        result = self.run_check(helper=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("src/Good.hs", output)
        self.assertIn("formatting", output)

    def test_unmerged_index_is_rejected_without_changing_it(self) -> None:
        object_id = self.git("hash-object", "-w", "--stdin", input=b"module Conflict where\n").strip()
        self.git("update-index", "--force-remove", "--", "src/Good.hs")
        self.git(
            "update-index", "--index-info",
            input=b"".join(b"100644 " + object_id + b" " + str(stage).encode()
                           + b"\tsrc/Good.hs\n" for stage in (1, 2, 3)),
        )
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("unmerged staged path: src/Good.hs", result.stderr)

    def stage_uninitialized_gitlink(self, path: str) -> None:
        # Generate an object ID without storing it or creating a submodule worktree.
        object_id: str = self.git("hash-object", "--stdin", input=path.encode()).decode().strip()
        self.git("update-index", "--add", "--cacheinfo", "160000", object_id, path)

    def test_staged_checks_accept_uninitialized_hs_pbt(self) -> None:
        self.stage_uninitialized_gitlink("hs-pbt")
        self.assertFalse((self.root / "hs-pbt").exists())
        result: subprocess.CompletedProcess[str] = self.run_check()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("HLint: checked", result.stdout)

    def test_gitlink_is_rejected_instead_of_omitting_its_tree(self) -> None:
        self.stage_uninitialized_gitlink("hs-pbt")
        self.stage_uninitialized_gitlink("vendor tree/hs-pbt")
        result: subprocess.CompletedProcess[str] = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("unsupported staged mode 160000: vendor tree/hs-pbt", result.stderr)

    def test_hook_uses_staged_drivers_even_when_working_drivers_are_broken(self) -> None:
        (self.root / "src" / "linked source.hs").symlink_to("Good.hs")
        self.git("add", "--", "src/linked source.hs")
        self.write("scripts/check-staged.sh", "this is not a shell script\n")
        self.write("scripts/staged_policy.py", "this is not Python\n")
        result = self.run_check(hook=True)
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("staged symlink is unsupported: src/linked source.hs", result.stderr)

    def test_bad_staged_source_cannot_be_replaced_by_a_working_tree_fix(self) -> None:
        good = (self.root / "src" / "Good.hs").read_text(encoding="utf-8")
        self.write("src/Good.hs", good.replace("\n\n", "\n\n-- " + "x" * 78 + "\n", 1))
        self.git("add", "--", "src/Good.hs")
        self.write("src/Good.hs", good)
        result = self.run_check(hook=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("src/Good.hs:3", output)
        self.assertRegex(output, r"line[-_ ]width")
        self.assertIn("81", output)
        self.assertIn("80", output)

    def test_good_staged_tree_ignores_bad_unstaged_sources_scripts_and_config(self) -> None:
        self.write("src/Good.hs", "this is not Haskell\n")
        self.write("src/untracked dir/Bad.hs", "not Haskell either\n")
        for name in ("check-source.sh", "check-shell.sh", "check-hlint.sh"):
            self.write("scripts/" + name, "this is not a shell script\n")
        for name in ("fourmolu.yaml", ".hlint.yaml", ".shellcheckrc", "scripts/source-policy.json"):
            self.write(name, "this is not valid policy configuration\n")
        result = self.run_check(hook=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("Staged source, shell, and HLint checks passed", result.stdout)

    def test_staged_shell_failure_cannot_be_replaced_by_unstaged_quotes(self) -> None:
        self.write("scripts/broken script.sh", "#!/bin/sh\nprintf '%s\\n' $1\n")
        self.git("add", "--", "scripts/broken script.sh")
        self.write("scripts/broken script.sh", "#!/bin/sh\nprintf '%s\\n' \"$1\"\n")
        result = self.run_check(hook=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("scripts/broken script.sh", output)
        self.assertIn("SC2086", output)
        self.assertRegex(output, r"(?:line 2|:2:)")

    def test_staged_directory_budget_checks_all_entries_including_missing_working_files(self) -> None:
        for index in range(20):
            self.write(f"src/budget dir/Entry{index:02}.hs", f"module Entry{index:02} where\n")
        self.git("add", "--", "src/budget dir")
        accepted = self.run_check()
        self.assertEqual(accepted.returncode, 0, accepted.stdout + accepted.stderr)
        extra = self.write("src/budget dir/Extra.hs", "module Extra where\n")
        self.git("add", "--", "src/budget dir/Extra.hs")
        extra.unlink()
        rejected = self.run_check(hook=True)
        output = rejected.stdout + rejected.stderr
        self.assertNotEqual(rejected.returncode, 0, output)
        self.assertIn("src/budget dir", output)
        self.assertRegex(output, r"directory[-_ ]entries")
        self.assertIn("21", output)
        self.assertIn("20", output)

    def test_staged_hlint_suggestion_is_not_hidden_by_unstaged_fix(self) -> None:
        template = "module Main where\n\nmain :: IO ()\nmain = pure ()\n\nempty :: [a] -> Bool\n"
        self.write("app/Main.hs", template + "empty xs = length xs == 0\n")
        self.git("add", "--", "app/Main.hs")
        self.write("app/Main.hs", template + "empty xs = null xs\n")
        result = self.run_check(hook=True)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0, output)
        self.assertIn("app/Main.hs:7", output)
        self.assertIn("Suggestion: Use null", output)

    def test_staged_formatter_config_cannot_be_replaced_by_unstaged_config(self) -> None:
        config = (self.root / "fourmolu.yaml").read_text(encoding="utf-8")
        self.write("fourmolu.yaml", "indentation: not-a-number\n")
        self.git("add", "--", "fourmolu.yaml")
        self.write("fourmolu.yaml", config)
        result = self.run_check()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("fourmolu.yaml", result.stdout + result.stderr)

    def test_full_check_finds_untracked_source_that_the_staged_check_excludes(self) -> None:
        self.write("src/new dir/Bad.hs", "module Bad where\n\n-- " + "x" * 78 + "\n")
        before = self.state()
        full = subprocess.run(
            ["sh", "scripts/check-source.sh"], cwd=self.root, env=self.env,
            text=True, capture_output=True, check=False, timeout=120,
        )
        after = self.state()
        self.assertEqual(after.index, before.index)
        self.assertEqual(after.effective_index, before.effective_index)
        for relative, entry in before.entries.items():
            self.assertEqual(after.entries.get(relative), entry)
        # Normal Python script imports may add bytecode artifacts. The hook's
        # separate run_check assertion still requires the entire tree unchanged.
        added: set[str] = after.entries.keys() - before.entries.keys()
        self.assertTrue(all("__pycache__" in Path(relative).parts for relative in added))
        self.assertNotEqual(full.returncode, 0, full.stdout + full.stderr)
        self.assertIn("src/new dir/Bad.hs:3", full.stdout + full.stderr)
        self.assertRegex(full.stdout + full.stderr, r"line[-_ ]width")
        staged = self.run_check()
        self.assertEqual(staged.returncode, 0, staged.stdout + staged.stderr)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ghc", default=os.environ.get("GHC"))
    parser.add_argument("--self-test", action="store_true", required=True)
    parser.add_argument("tests", nargs="*", help="optional unittest method names")
    arguments = Arguments()
    parser.parse_args(namespace=arguments)
    StagedPolicyTests.ghc = arguments.ghc
    suite = (
        unittest.TestSuite(StagedPolicyTests(name) for name in arguments.tests)
        if arguments.tests
        else unittest.defaultTestLoader.loadTestsFromTestCase(StagedPolicyTests)
    )
    return 0 if unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
