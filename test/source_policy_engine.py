#!/usr/bin/env python3
"""Regression tests for the path-taking GHC source-metrics engine."""

import argparse
from collections.abc import Mapping, Sequence
import json
import os
from pathlib import Path
import subprocess
import tempfile
from typing import Final, TypedDict, cast
import unittest


ROOT: Final[Path] = Path(__file__).resolve().parents[1]
SPECIMENS: Final[Path] = ROOT / "test" / "source-policy"


class Location(TypedDict):
    line: int
    column: int
    name: str


class DefinitionMetric(Location):
    lines: int


class StringMetric(TypedDict):
    line: int
    column: int
    value: str


class FileMetrics(TypedDict):
    path: str
    code_lines: list[int]
    top_level: list[Location]
    definitions: list[DefinitionMetric]
    strings: list[StringMetric]


class Arguments(argparse.Namespace):
    ghc: str
    self_test: bool


def fields(raw: object, keys: set[str]) -> dict[str, object]:
    if not isinstance(raw, dict):
        raise ValueError("expected JSON object")
    result: dict[str, object] = {}
    for key, value in cast(dict[object, object], raw).items():
        if not isinstance(key, str):
            raise ValueError("expected string key")
        result[key] = value
    if set(result) != keys:
        raise ValueError("unexpected JSON fields")
    return result


def items(raw: object) -> list[object]:
    if not isinstance(raw, list):
        raise ValueError("expected JSON array")
    return list(cast(list[object], raw))


def integer(raw: object) -> int:
    if not isinstance(raw, int) or isinstance(raw, bool):
        raise ValueError("expected JSON integer")
    return raw


def text(raw: object) -> str:
    if not isinstance(raw, str):
        raise ValueError("expected JSON string")
    return raw


def location(raw: object) -> Location:
    row = fields(raw, {"line", "column", "name"})
    return {"line": integer(row["line"]), "column": integer(row["column"]),
            "name": text(row["name"])}


def definition(raw: object) -> DefinitionMetric:
    row = fields(raw, {"line", "column", "name", "lines"})
    return {"line": integer(row["line"]), "column": integer(row["column"]),
            "name": text(row["name"]), "lines": integer(row["lines"])}


def string_metric(raw: object) -> StringMetric:
    row = fields(raw, {"line", "column", "value"})
    return {"line": integer(row["line"]), "column": integer(row["column"]),
            "value": text(row["value"])}


def decode_metrics(encoded: str) -> FileMetrics:
    raw: object = json.loads(encoded)
    row = fields(raw, {"path", "code_lines", "top_level", "definitions", "strings"})
    return {"path": text(row["path"]),
            "code_lines": [integer(value) for value in items(row["code_lines"])],
            "top_level": [location(value) for value in items(row["top_level"])],
            "definitions": [definition(value) for value in items(row["definitions"])],
            "strings": [string_metric(value) for value in items(row["strings"])]}


def temporary_directory(prefix: str) -> tempfile.TemporaryDirectory[str]:
    return tempfile.TemporaryDirectory(prefix=prefix)


def build_engine(ghc: str, outputdir: str | Path) -> Path:
    """Build with the caller's validated GHC; return the standalone executable."""
    directory = Path(outputdir)
    directory.mkdir(parents=True, exist_ok=True)
    executable = directory / "source-policy-engine"
    subprocess.run(
        [ghc, "-v0", "--make", "-package", "ghc", "-dynamic", "-Wall", "-Werror",
         "-main-is", "SourcePolicy.main", "-outputdir", str(directory),
         "-o", str(executable), str(ROOT / "test" / "SourcePolicy.hs")],
        check=True,
    )
    return executable


class EngineTests(unittest.TestCase):
    executable: Path
    libdir: str

    def invoke(self, root: Path, paths: Sequence[str]) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [str(self.executable), self.libdir, *paths],
            cwd=root, text=True, capture_output=True, check=False,
        )

    def run_files(
        self, files: Mapping[str, str | bytes], order: Sequence[str] | None = None
    ) -> subprocess.CompletedProcess[str]:
        with temporary_directory("source-policy-input-") as temporary:
            root = Path(temporary)
            for name, content in files.items():
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(content.encode("utf-8") if isinstance(content, str) else content)
            return self.invoke(root, list(order or files))

    def scan(self, specimen: str) -> FileMetrics:
        result = self.run_files({"Sample.hs": (SPECIMENS / specimen).read_bytes()})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "")
        rows = [decode_metrics(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), 1)
        return rows[0]

    def rejects(self, source: str | bytes, diagnostic: str, name: str = "Sample.hs") -> None:
        first = self.run_files({name: source})
        second = self.run_files({name: source})
        self.assertNotEqual(first.returncode, 0, first.stdout + first.stderr)
        self.assertEqual(first.stdout, "", "a failed batch must not publish partial metrics")
        self.assertEqual(first.stderr, diagnostic + "\n")
        self.assertEqual((second.returncode, second.stdout, second.stderr),
                         (first.returncode, first.stdout, first.stderr))

    def test_basic_physical_metrics_and_separate_signature(self) -> None:
        self.assertEqual(self.scan("basic.txt"), {
            "path": "Sample.hs", "code_lines": [1, 3, 4],
            "top_level": [{"line": 4, "column": 1, "name": "value"}],
            "definitions": [{"line": 4, "column": 1, "name": "value", "lines": 1}],
            "strings": [],
        })

    def test_structural_definitions_are_grouped_and_scope_sensitive(self) -> None:
        result = self.scan("structure.txt")
        self.assertEqual(result["code_lines"], [
            1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
            17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
        ])
        self.assertEqual(result["top_level"], [
            {"line": 4, "column": 1, "name": "f"},
            {"line": 17, "column": 1, "name": "<+>"},
            {"line": 18, "column": 1, "name": "<pattern>"},
            {"line": 19, "column": 1, "name": "constant"},
        ])
        self.assertEqual(result["definitions"], [
            {"line": 4, "column": 1, "name": "f", "lines": 12},
            {"line": 10, "column": 5, "name": "local", "lines": 6},
            {"line": 11, "column": 11, "name": "same", "lines": 1},
            {"line": 12, "column": 11, "name": "inner", "lines": 3},
            {"line": 13, "column": 17, "name": "same", "lines": 1},
            {"line": 17, "column": 1, "name": "<+>", "lines": 1},
            {"line": 18, "column": 1, "name": "<pattern>", "lines": 1},
            {"line": 19, "column": 1, "name": "constant", "lines": 1},
            {"line": 22, "column": 3, "name": "method", "lines": 3},
            {"line": 23, "column": 9, "name": "local", "lines": 1},
            {"line": 26, "column": 3, "name": "method", "lines": 3},
            {"line": 27, "column": 6, "name": "<lambda>", "lines": 2},
        ])

    def test_decoded_strings_include_all_haskell_contexts_once(self) -> None:
        result = self.scan("literals.txt")
        self.assertEqual(result["strings"], [
            {"line": 4, "column": 8, "value": ""},
            {"line": 4, "column": 12, "value": "a"},
            {"line": 4, "column": 17, "value": "a"},
            {"line": 5, "column": 7, "value": "a"},
            {"line": 5, "column": 13, "value": "matched"},
            {"line": 6, "column": 14, "value": "a"},
            {"line": 7, "column": 17, "value": "a"},
            {"line": 8, "column": 11, "value": "a"},
            {"line": 9, "column": 11, "value": "a"},
            {"line": 10, "column": 11, "value": "a"},
            {"line": 11, "column": 21, "value": "a"},
        ])
        self.assertIn({"line": 11, "column": 12, "name": "nested", "lines": 1},
                      result["definitions"])
        self.assertNotIn({"line": 11, "column": 12, "name": "nested"},
                         result["top_level"])

    def test_code_tokens_preserve_pragmas_gaps_unicode_and_physical_lines(self) -> None:
        source = (SPECIMENS / "lexical.txt").read_bytes()
        expected: FileMetrics = {
            "path": "Sample.hs",
            "code_lines": [1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26],
            "top_level": [
                {"line": 15, "column": 1, "name": "a"},
                {"line": 16, "column": 1, "name": "b"},
                {"line": 17, "column": 1, "name": "c"},
                {"line": 18, "column": 1, "name": "gap"},
                {"line": 21, "column": 1, "name": "value"},
                {"line": 25, "column": 1, "name": "tabs"},
                {"line": 26, "column": 1, "name": "unicode"},
            ],
            "definitions": [
                {"line": 15, "column": 1, "name": "a", "lines": 1},
                {"line": 16, "column": 1, "name": "b", "lines": 1},
                {"line": 17, "column": 1, "name": "c", "lines": 1},
                {"line": 18, "column": 1, "name": "gap", "lines": 3},
                {"line": 21, "column": 1, "name": "value", "lines": 3},
                {"line": 22, "column": 8, "name": "x", "lines": 1},
                {"line": 25, "column": 1, "name": "tabs", "lines": 1},
                {"line": 26, "column": 1, "name": "unicode", "lines": 1},
            ],
            "strings": [
                {"line": 15, "column": 5, "value": "-- {- -}"},
                {"line": 16, "column": 5, "value": "λ🙂"},
                {"line": 17, "column": 5, "value": "a"},
                {"line": 18, "column": 7, "value": "ab"},
                {"line": 25, "column": 9, "value": "a"},
                {"line": 26, "column": 12, "value": "λ🙂"},
                {"line": 26, "column": 18, "value": "z"},
            ],
        }
        with_trailing_space = source.replace(b'tabs =\t"a"\n', b'tabs =\t"a"\t  \n')
        self.assertNotEqual(with_trailing_space, source)
        endings = [ending for original in (source, with_trailing_space)
                   for ending in (original, original.replace(b"\n", b"\r\n"), original.rstrip(b"\n"))]
        for ending in endings:
            with self.subTest(ending=repr(ending[-10:])):
                result = self.run_files({"Sample.hs": ending})
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(decode_metrics(result.stdout), expected)

    def test_actual_location_directives_are_rejected_at_physical_locations(self) -> None:
        for directive in ('{-# LINE 1 "fiction.hs" #-}', '{-# COLUMN 50 #-}',
                          '#line 1 "fiction.hs"', '# 1 "fiction.hs"'):
            with self.subTest(directive=directive):
                self.rejects("module Sample where\n" + directive + "\nf = 1\n",
                             "Sample.hs:2:1: unsupported source-location directive")

    def test_quoted_and_commented_directives_do_not_change_physical_locations(self) -> None:
        self.assertEqual(self.scan("quoted-directives.txt"), {
            "path": "Sample.hs", "code_lines": [1, 2, 3, 9, 10, 11, 12, 13, 15],
            "top_level": [
                {"line": 3, "column": 1, "name": "text"},
                {"line": 9, "column": 1, "name": "quoted"},
                {"line": 13, "column": 1, "name": "f"},
                {"line": 15, "column": 1, "name": "g"},
            ],
            "definitions": [
                {"line": 3, "column": 1, "name": "text", "lines": 1},
                {"line": 9, "column": 1, "name": "quoted", "lines": 4},
                {"line": 13, "column": 1, "name": "f", "lines": 1},
                {"line": 15, "column": 1, "name": "g", "lines": 1},
            ],
            "strings": [{"line": 3, "column": 8, "value": '{-# LINE 1 "fake" #-}'}],
        })

    def test_primitive_byte_strings_fail_instead_of_comparing_text(self) -> None:
        self.rejects('{-# LANGUAGE MagicHash #-}\nmodule Sample where\nx = "a"#\n',
                     "Sample.hs:3:5: unsupported primitive byte-string literal")

    def test_preprocessing_is_rejected_without_running_external_tools(self) -> None:
        for pragma, diagnostic in (
            ("{-# LANGUAGE CPP #-}", "unsupported preprocessing (CPP)"),
            ("{-# OPTIONS_GHC -F #-}", "unsupported preprocessing (-F)"),
            ('{-# OPTIONS_GHC -pgmF "does-not-exist" #-}',
             "unsupported preprocessing (-pgmF)"),
        ):
            with self.subTest(pragma=pragma):
                self.rejects(pragma + "\nmodule Sample where\nf = 1\n",
                             "Sample.hs: " + diagnostic)
        for directive in ("#if 1", "#ifdef FLAG", "#define FLAG 1", '#include "x.h"',
                          "#pragma once", '#warning "no"', '#! /usr/bin/env runghc',
                          '{-# INCLUDE "x.h" #-}'):
            with self.subTest(directive=directive):
                self.rejects("module Sample where\n" + directive + "\nf = 1\n",
                             "Sample.hs:2:1: unsupported preprocessing directive")

    def test_invalid_encoding_and_unfamiliar_inputs_fail_cleanly(self) -> None:
        self.rejects(b"module Sample where\nx = \xff\n", "Sample.hs: invalid UTF-8")
        for name in ("Sample.lhs", "Sample.hsc", "Sample.hs-boot", "Sample.hsig"):
            with self.subTest(name=name):
                self.rejects("module Sample where\nx = 1\n",
                             name + ": unsupported source input (expected .hs)", name=name)
        result = self.run_files({}, order=["Missing.hs"])
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "Missing.hs: cannot read source file\n")

    def test_parse_lex_and_header_failures_have_deterministic_physical_diagnostics(self) -> None:
        for source, diagnostic in (
            ("module Sample where\nf =\n", "Sample.hs:3:1: cannot parse Haskell"),
            ('module Sample where\nf = "oops\n', "Sample.hs:2:10: cannot lex Haskell"),
            ("{-# LANGUAGE NotReal #-}\nmodule Sample where\nf = 1\n",
             "Sample.hs:1:14: cannot parse Haskell pragmas"),
            ("{-# OPTIONS_GHC -not-real-flag #-}\nmodule Sample where\nf = 1\n",
             "Sample.hs: unrecognized Haskell file options"),
            ("{-# OPTIONS_GHC -fmax-relevant-binds=oops #-}\nmodule Sample where\nf = 1\n",
             "Sample.hs: cannot apply Haskell file options"),
        ):
            with self.subTest(diagnostic=diagnostic):
                self.rejects(source, diagnostic)

    def test_engine_rejects_external_paths_and_symlink_components(self) -> None:
        for name in ("../Sample.hs", str(SPECIMENS / "basic.txt")):
            result = self.run_files({}, order=[name])
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.stderr,
                             "source-policy-engine: expected repository-relative paths without '..'\n")
        with temporary_directory("source-policy-links-") as temporary:
            root = Path(temporary)
            (root / "Actual.hs").write_text("module Sample where\nf = 1\n", encoding="utf-8")
            (root / "nested").mkdir()
            (root / "nested" / "Other.hs").write_text("x = 1\n", encoding="utf-8")
            (root / "Link.hs").symlink_to("Actual.hs")
            (root / "Alias").symlink_to("nested", target_is_directory=True)
            (root / "Broken.hs").symlink_to("Absent.hs")
            for name in ("Link.hs", "Alias/Other.hs", "Broken.hs"):
                with self.subTest(name=name):
                    result = self.invoke(root, [name])
                    self.assertNotEqual(result.returncode, 0)
                    self.assertEqual(result.stdout, "")
                    self.assertEqual(result.stderr, name + ": unsupported symbolic link\n")

    def test_bad_ghc_libdir_fails_without_leaking_environment_paths(self) -> None:
        with temporary_directory("source-policy-libdir-") as temporary:
            root = Path(temporary)
            (root / "Sample.hs").write_text("f = 1\n", encoding="utf-8")
            result = subprocess.run(
                [str(self.executable), str(root / "missing-libdir"), "Sample.hs"],
                cwd=root, text=True, capture_output=True, check=False,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.stderr, "source-policy-engine: cannot initialize GHC\n")

    def test_ghc2021_file_extensions_and_unavailable_imports(self) -> None:
        self.assertEqual(self.scan("language.txt"), {
            "path": "Sample.hs", "code_lines": [1, 2, 3, 4, 5, 6],
            "top_level": [{"line": 5, "column": 1, "name": "boxed"},
                          {"line": 6, "column": 1, "name": "uses"}],
            "definitions": [{"line": 5, "column": 1, "name": "boxed", "lines": 1},
                            {"line": 6, "column": 1, "name": "uses", "lines": 1}],
            "strings": [],
        })
        self.rejects("{-# LANGUAGE NoBangPatterns #-}\nmodule Sample where\nboxed !x = x\n",
                     "Sample.hs:3:7: cannot parse Haskell")
        self.assertEqual(self.scan("lambdas.txt"), {
            "path": "Sample.hs", "code_lines": [1, 2, 3, 4, 5, 6],
            "top_level": [{"line": 3, "column": 1, "name": "choose"}],
            "definitions": [
                {"line": 3, "column": 1, "name": "choose", "lines": 4},
                {"line": 3, "column": 10, "name": "<lambda>", "lines": 4},
                {"line": 4, "column": 8, "name": "<lambda>", "lines": 1},
                {"line": 5, "column": 8, "name": "<lambda>", "lines": 2},
            ],
            "strings": [],
        })

    def test_empty_files_layout_tokens_and_same_line_definitions(self) -> None:
        for source in ("", "\n-- comment\n{- nested {- inner -} -}\n\n"):
            result = self.run_files({"Sample.hs": source})
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(decode_metrics(result.stdout), {
                "path": "Sample.hs", "code_lines": [], "top_level": [],
                "definitions": [], "strings": [],
            })
        result = self.run_files({"Sample.hs": "module S where { f = 1; g = \\x -> x }\n"})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(decode_metrics(result.stdout), {
            "path": "Sample.hs", "code_lines": [1],
            "top_level": [{"line": 1, "column": 18, "name": "f"},
                          {"line": 1, "column": 25, "name": "g"}],
            "definitions": [
                {"line": 1, "column": 18, "name": "f", "lines": 1},
                {"line": 1, "column": 25, "name": "g", "lines": 1},
                {"line": 1, "column": 29, "name": "<lambda>", "lines": 1},
            ],
            "strings": [],
        })

    def test_factual_counts_do_not_enforce_parent_thresholds(self) -> None:
        for size in (50, 51, 499, 500):
            source = "module Sample where\nf =\n  [ 0\n" + "  , 1\n" * (size - 3) + "  ]\n"
            with self.subTest(definition_lines=size):
                result = self.run_files({"Sample.hs": source})
                self.assertEqual(result.returncode, 0, result.stderr)
                metrics = decode_metrics(result.stdout)
                self.assertEqual(metrics["code_lines"], list(range(1, size + 2)))
                self.assertEqual(metrics["top_level"], [{"line": 2, "column": 1, "name": "f"}])
                self.assertEqual(metrics["definitions"], [
                    {"line": 2, "column": 1, "name": "f", "lines": size},
                ])
        for count in (20, 21):
            source = "module Sample where\n" + "".join(f"f{i} = {i}\n" for i in range(count))
            with self.subTest(top_level=count):
                result = self.run_files({"Sample.hs": source})
                self.assertEqual(result.returncode, 0, result.stderr)
                metrics = decode_metrics(result.stdout)
                self.assertEqual(metrics["code_lines"], list(range(1, count + 2)))
                self.assertEqual(metrics["top_level"], [
                    {"line": i + 2, "column": 1, "name": f"f{i}"} for i in range(count)
                ])
        for source, expected in (
            ('values = ["a", "\\97"]\n', [
                {"line": 1, "column": 11, "value": "a"},
                {"line": 1, "column": 16, "value": "a"},
            ]),
            ('values = ["a", "\\97", "\\x61"]\n', [
                {"line": 1, "column": 11, "value": "a"},
                {"line": 1, "column": 16, "value": "a"},
                {"line": 1, "column": 23, "value": "a"},
            ]),
        ):
            result = self.run_files({"Sample.hs": source})
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(decode_metrics(result.stdout)["strings"], expected)

    def test_batch_order_is_canonical_and_failure_is_atomic(self) -> None:
        sources: dict[str, str | bytes] = {
            "Z.hs": "z = 0\n", "nested/A.hs": "a = 1\n", "λ.hs": 'u = "λ"\n',
        }
        first = self.run_files(sources, order=["λ.hs", "Z.hs", "nested/A.hs"])
        second = self.run_files(sources, order=["nested/A.hs", "./Z.hs", "λ.hs", "Z.hs"])
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertEqual((first.returncode, first.stdout, first.stderr),
                         (second.returncode, second.stdout, second.stderr))
        self.assertEqual([decode_metrics(row)["path"] for row in first.stdout.splitlines()],
                         ["Z.hs", "nested/A.hs", "λ.hs"])
        for order in (["Bad.hs", "Good.hs"], ["Good.hs", "Bad.hs"]):
            result = self.run_files({"Bad.hs": "x =\n", "Good.hs": "x = 1\n"}, order=order)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.stderr, "Bad.hs:2:1: cannot parse Haskell\n")

    def test_named_implicit_locals_and_command_lambdas_are_not_omitted(self) -> None:
        self.assertEqual(self.scan("local-lambdas.txt"), {
            "path": "Sample.hs", "code_lines": [1, 2, 3, 4, 5, 6, 7, 8, 9],
            "top_level": [{"line": 3, "column": 1, "name": "value"},
                          {"line": 7, "column": 1, "name": "arrow"}],
            "definitions": [
                {"line": 3, "column": 1, "name": "value", "lines": 4},
                {"line": 4, "column": 7, "name": "?config", "lines": 2},
                {"line": 7, "column": 1, "name": "arrow", "lines": 3},
                {"line": 8, "column": 4, "name": "<lambda>", "lines": 2},
            ],
            "strings": [{"line": 5, "column": 9, "value": "v"}],
        })

    def test_pattern_synonyms_fail_explicitly_pending_counting_policy(self) -> None:
        self.rejects("{-# LANGUAGE PatternSynonyms #-}\nmodule Sample where\npattern P x = Just x\n",
                     "Sample.hs:3:1: unsupported pattern-synonym binding")

    def test_json_preserves_decoded_control_characters_and_escapes(self) -> None:
        result = self.run_files({"Sample.hs": r'controls = "\0\n\t\"\\\DEL\128512"' + "\n"})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(decode_metrics(result.stdout), {
            "path": "Sample.hs", "code_lines": [1],
            "top_level": [{"line": 1, "column": 1, "name": "controls"}],
            "definitions": [{"line": 1, "column": 1, "name": "controls", "lines": 1}],
            "strings": [{"line": 1, "column": 12, "value": '\0\n\t"\\\x7f😀'}],
        })

    def test_unreadable_files_and_directories_fail_without_partial_metrics(self) -> None:
        with temporary_directory("source-policy-unreadable-") as temporary:
            root = Path(temporary)
            (root / "Directory.hs").mkdir()
            result = self.invoke(root, ["Directory.hs"])
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.stderr, "Directory.hs: unsupported non-regular source file\n")
            path = root / "Unreadable.hs"
            path.write_text("value = 1\n", encoding="utf-8")
            path.chmod(0)
            try:
                if os.access(path, os.R_OK):
                    self.skipTest("this account can read mode-000 files")
                result = self.invoke(root, ["Unreadable.hs"])
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "Unreadable.hs: cannot read source file\n")
            finally:
                path.chmod(0o600)

    def test_grouped_shadowed_local_equations_and_local_patterns(self) -> None:
        self.assertEqual(self.scan("local-groups.txt"), {
            "path": "Sample.hs", "code_lines": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
            "top_level": [{"line": 2, "column": 1, "name": "f"},
                          {"line": 9, "column": 1, "name": "patternValue"}],
            "definitions": [
                {"line": 2, "column": 1, "name": "f", "lines": 7},
                {"line": 3, "column": 7, "name": "g", "lines": 5},
                {"line": 5, "column": 13, "name": "g", "lines": 2},
                {"line": 9, "column": 1, "name": "patternValue", "lines": 4},
                {"line": 10, "column": 7, "name": "<pattern>", "lines": 2},
            ],
            "strings": [],
        })

    def test_special_source_files_fail_instead_of_blocking_on_reads(self) -> None:
        with temporary_directory("source-policy-special-") as temporary:
            root = Path(temporary)
            os.mkfifo(root / "Pipe.hs")
            result = subprocess.run(
                [str(self.executable), self.libdir, "Pipe.hs"], cwd=root,
                text=True, capture_output=True, check=False, timeout=5,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertEqual(result.stderr, "Pipe.hs: unsupported non-regular source file\n")

    def test_unicode_line_separators_cannot_split_protocol_records(self) -> None:
        result = self.run_files({"Sample.hs": r'separators = "\x85\x2028\x2029"' + "\n"})
        self.assertEqual(result.returncode, 0, result.stderr)
        rows = result.stdout.splitlines()
        self.assertEqual(len(rows), 1)
        self.assertEqual(decode_metrics(rows[0])["strings"], [
            {"line": 1, "column": 14, "value": "\x85\u2028\u2029"},
        ])


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ghc", default=os.environ.get("GHC", "ghc"))
    parser.add_argument("--self-test", action="store_true", required=True)
    arguments = parser.parse_args(namespace=Arguments())
    with temporary_directory("source-policy-engine-") as temporary:
        EngineTests.executable = build_engine(arguments.ghc, temporary)
        EngineTests.libdir = subprocess.check_output(
            [arguments.ghc, "--print-libdir"], text=True
        ).strip()
        suite = unittest.defaultTestLoader.loadTestsFromTestCase(EngineTests)
        return 0 if unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
