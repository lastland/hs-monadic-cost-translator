#!/usr/bin/env python3

import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SPECIMENS = ROOT / "test" / "comment-policy"
PIN = "9.10.3"
EXCLUDED = {".stack-work", "dist", "dist-newstyle", ".git", ".scratch", "hs-pbt"}


def translator_sources(root):
    candidates = [path.relative_to(root) for directory in ("src", "app")
                  for path in (root / directory).rglob("*.hs")]
    return sorted(path for path in candidates if not EXCLUDED.intersection(path.parts))


class CommentPolicyTests(unittest.TestCase):
    executable = None
    libdir = None
    ghc = None

    def scan(self, specimen, *options):
        return subprocess.run(
            [str(self.executable), self.libdir, *options, str(SPECIMENS / specimen)],
            text=True,
            capture_output=True,
            check=False,
        )

    def test_ordinary_line_comment_is_rejected(self):
        result = self.scan("line.txt", "--check")
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("explanatory comment", result.stderr)

    def test_literals_pragmas_cpp_and_shebang_are_not_comments(self):
        result = self.scan("noncomments.txt", "--check")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_block_nested_and_haddock_comments_are_rejected(self):
        for specimen in ("block.txt", "nested.txt", "haddock-line.txt", "haddock-block.txt"):
            with self.subTest(specimen=specimen):
                result = self.scan(specimen, "--check")
                self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("explanatory comment", result.stderr)

    def test_invalid_parser_input_can_still_be_checked(self):
        result = self.scan("malformed.txt", "--check")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_source_distribution_checks_only_translator_implementation(self):
        with tempfile.TemporaryDirectory(prefix="comment-policy-source-") as temporary:
            root = Path(temporary)
            for relative in ("src/Cost/Translate.hs", "app/Main.hs", "app/Nested/Other.hs"):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes((SPECIMENS / "noncomments.txt").read_bytes())
            for relative in (
                "test/Fixture.hs", "test/cases/example/Example.hs", "examples/Generated.hs",
                "generated/Published.hs", "integration/Main.hs",
                "hs-pbt/Runtime.hs", "src/.stack-work/Generated.hs",
                "test/example.py", "cost-translate.cabal",
            ):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes((SPECIMENS / "line.txt").read_bytes())
            command = [sys.executable, str(Path(__file__).resolve()),
                       "--ghc", self.ghc, "--root", str(root)]
            clean = subprocess.run(command, text=True, capture_output=True, check=False)
            self.assertEqual(clean.returncode, 0, clean.stdout + clean.stderr)
            self.assertIn("3 translator source files", clean.stdout)
            (root / "src" / "Cost" / "Translate.hs").write_bytes(
                (SPECIMENS / "line.txt").read_bytes()
            )
            rejected = subprocess.run(command, text=True, capture_output=True, check=False)
            self.assertNotEqual(rejected.returncode, 0, rejected.stdout + rejected.stderr)
            self.assertIn("explanatory comment", rejected.stderr)
            self.assertIn("src/Cost/Translate.hs:2", rejected.stderr)

    def test_new_translator_sources_are_covered(self):
        with tempfile.TemporaryDirectory(prefix="comment-policy-checkout-") as temporary:
            root = Path(temporary)
            (root / ".git").mkdir()
            for relative, specimen in (
                ("src/Kept.hs", "noncomments.txt"), ("app/Main.hs", "noncomments.txt"),
                ("test/Fixture.hs", "line.txt"),
            ):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes((SPECIMENS / specimen).read_bytes())
            result = subprocess.run(
                [sys.executable, str(Path(__file__).resolve()), "--ghc", self.ghc,
                 "--root", str(root)],
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("2 translator source files", result.stdout)
            for relative in ("src/Cost/New.hs", "app/New.hs", "app/Nested/New.hs"):
                with self.subTest(path=relative):
                    path = root / relative
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes((SPECIMENS / "line.txt").read_bytes())
                    try:
                        rejected = subprocess.run(
                            [sys.executable, str(Path(__file__).resolve()), "--ghc", self.ghc,
                             "--root", str(root)],
                            text=True,
                            capture_output=True,
                            check=False,
                        )
                        self.assertNotEqual(rejected.returncode, 0, rejected.stdout + rejected.stderr)
                        self.assertIn(relative + ":2", rejected.stderr)
                        self.assertIn("explanatory comment", rejected.stderr)
                    finally:
                        path.unlink()


def main():
    parser = argparse.ArgumentParser(description="Reject comments in translator implementation sources.")
    parser.add_argument("--ghc", default=os.environ.get("GHC", "ghc"))
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--self-test", action="store_true", help="run scanner and scope regressions")
    parser.add_argument("--check", action="store_true", help="also check sources after self-tests")
    arguments = parser.parse_args()
    version = subprocess.check_output([arguments.ghc, "--numeric-version"], text=True).strip()
    if version != PIN:
        parser.error("unsupported GHC " + version + "; required GHC " + PIN)
    with tempfile.TemporaryDirectory(prefix="comment-policy-") as temporary:
        directory = Path(temporary)
        executable = directory / "comment-policy"
        subprocess.run(
            [arguments.ghc, "-v0", "--make", "-package", "ghc", "-dynamic",
             "-Wall", "-Werror", "-main-is", "CommentPolicy.main",
             "-outputdir", str(directory), "-o", str(executable),
             str(ROOT / "test" / "CommentPolicy.hs")],
            check=True,
        )
        libdir = subprocess.check_output([arguments.ghc, "--print-libdir"], text=True).strip()
        if arguments.self_test:
            CommentPolicyTests.executable = executable
            CommentPolicyTests.libdir = libdir
            CommentPolicyTests.ghc = arguments.ghc
            suite = unittest.defaultTestLoader.loadTestsFromTestCase(CommentPolicyTests)
            if not unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful():
                return 1
        if not arguments.self_test or arguments.check:
            root = arguments.root.resolve()
            sources = translator_sources(root)
            if not sources:
                parser.error("no translator source files found in " + str(root))
            result = subprocess.run(
                [str(executable), libdir, "--check", *(str(path) for path in sources)],
                cwd=root,
                check=False,
            )
            if result.returncode:
                return result.returncode
            print("Comment policy: checked " + str(len(sources)) + " translator source files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
