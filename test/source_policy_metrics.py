"""Validated GHC facts and deterministic, configuration-driven policy reports."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
import json
from typing import cast


class FactsError(ValueError):
    """The facts engine did not account for the requested physical source."""


@dataclass(frozen=True)
class NamedFact:
    line: int
    column: int
    name: str


@dataclass(frozen=True)
class DefinitionFact:
    line: int
    column: int
    name: str
    lines: int


@dataclass(frozen=True)
class StringFact:
    line: int
    column: int
    value: str


@dataclass(frozen=True)
class FileFacts:
    path: str
    code_lines: tuple[int, ...]
    top_level: tuple[NamedFact, ...]
    definitions: tuple[DefinitionFact, ...]
    strings: tuple[StringFact, ...]


@dataclass(frozen=True, order=True)
class Violation:
    path: str
    line: int
    column: int
    rule: str
    actual: int
    limit: int

    def render(self) -> str:
        path = self.path if all(character.isprintable() for character in self.path) else repr(self.path)
        return (path + ":" + str(self.line) + ":" + str(self.column) + ": "
                + self.rule + ": actual " + str(self.actual) + ", limit " + str(self.limit))


def physical_lines(source: str) -> tuple[str, ...]:
    """Split only LF/CRLF delimiters; preserve every other source character."""
    pieces = source.split("\n")
    lines = [piece.removesuffix("\r") for piece in pieces[:-1]]
    if pieces[-1]:
        lines.append(pieces[-1])
    return tuple(lines)


def physical_width(line: str) -> int:
    width = 0
    for character in line:
        width = width + (8 - width % 8) if character == "\t" else width + 1
    return width


def _object(value: object, expected: frozenset[str], label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise FactsError(label + ": expected object")
    fields: dict[str, object] = {}
    for key, item in cast(dict[object, object], value).items():
        if not isinstance(key, str):
            raise FactsError(label + ": expected string keys")
        fields[key] = item
    if frozenset(fields) != expected:
        raise FactsError(label + ": unexpected or missing fields")
    return fields


def _array(value: object, label: str) -> list[object]:
    if not isinstance(value, list):
        raise FactsError(label + ": expected array")
    return cast(list[object], value)


def _positive(value: object, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value < 1:
        raise FactsError(label + ": expected positive integer")
    return value


def _string(value: object, label: str) -> str:
    if not isinstance(value, str):
        raise FactsError(label + ": expected string")
    return value


def _position(fields: Mapping[str, object], lines: tuple[str, ...], label: str) -> tuple[int, int]:
    line = _positive(fields["line"], label + " line")
    column = _positive(fields["column"], label + " column")
    if line > len(lines) or column > physical_width(lines[line - 1]) + 1:
        raise FactsError(label + ": position is outside physical source")
    return line, column


def _json_object(pairs: list[tuple[str, object]]) -> dict[str, object]:
    fields: dict[str, object] = {}
    for key, value in pairs:
        if key in fields:
            raise FactsError("duplicate JSON field")
        fields[key] = value
    return fields


def decode_facts(output: str, files: Mapping[str, str]) -> dict[str, FileFacts]:
    """Require exactly one complete, well-typed record per requested source file."""
    decoded: dict[str, FileFacts] = {}
    for record in output.splitlines():
        try:
            value: object = json.loads(record, object_pairs_hook=_json_object)
        except (ValueError, RecursionError) as problem:
            raise FactsError("invalid facts JSON") from problem
        fields = _object(value, frozenset({"path", "code_lines", "top_level", "definitions", "strings"}), "facts")
        path = _string(fields["path"], "facts path")
        if path not in files or path in decoded:
            raise FactsError("unexpected or duplicate source path in facts")
        lines = physical_lines(files[path])
        code_lines = tuple(_positive(item, "code line") for item in _array(fields["code_lines"], "code_lines"))
        if tuple(sorted(set(code_lines))) != code_lines or any(line > len(lines) for line in code_lines):
            raise FactsError("code_lines: expected unique, ordered physical source lines")
        top_level: list[NamedFact] = []
        for item in _array(fields["top_level"], "top_level"):
            declaration = _object(item, frozenset({"line", "column", "name"}), "top_level")
            line, column = _position(declaration, lines, "top_level")
            top_level.append(NamedFact(line, column, _string(declaration["name"], "top_level name")))
        definitions: list[DefinitionFact] = []
        for item in _array(fields["definitions"], "definitions"):
            definition = _object(item, frozenset({"line", "column", "name", "lines"}), "definition")
            line, column = _position(definition, lines, "definition")
            count = _positive(definition["lines"], "definition lines")
            if count > len(code_lines):
                raise FactsError("definition: code count exceeds file code lines")
            definitions.append(DefinitionFact(line, column, _string(definition["name"], "definition name"), count))
        strings: list[StringFact] = []
        for item in _array(fields["strings"], "strings"):
            literal = _object(item, frozenset({"line", "column", "value"}), "string")
            line, column = _position(literal, lines, "string")
            strings.append(StringFact(line, column, _string(literal["value"], "string value")))
        decoded[path] = FileFacts(path, code_lines, tuple(top_level), tuple(definitions), tuple(strings))
    if set(decoded) != set(files):
        raise FactsError("missing source file facts")
    return decoded


def measure(
    files: Mapping[str, str], directories: Mapping[str, int],
    facts: Mapping[str, FileFacts], limits: Mapping[str, int],
) -> list[Violation]:
    """Apply the maintained limits to validated facts and original physical text."""
    violations: list[Violation] = []
    for path, entries in directories.items():
        if entries > limits["directory_entries"]:
            violations.append(Violation(path, 1, 1, "directory_entries", entries, limits["directory_entries"]))
    for path, source in files.items():
        file_facts = facts[path]
        code_count = len(file_facts.code_lines)
        if code_count > limits["file_lines"]:
            violations.append(Violation(path, file_facts.code_lines[limits["file_lines"]], 1,
                                        "file_lines", code_count, limits["file_lines"]))
        declarations = sorted(file_facts.top_level, key=lambda declaration: (declaration.line, declaration.column, declaration.name))
        if len(declarations) > limits["top_level"]:
            declaration = declarations[limits["top_level"]]
            violations.append(Violation(path, declaration.line, declaration.column,
                                        "top_level", len(declarations), limits["top_level"]))
        for definition in file_facts.definitions:
            if definition.lines > limits["definition_lines"]:
                violations.append(Violation(path, definition.line, definition.column,
                                            "definition_lines", definition.lines, limits["definition_lines"]))
        occurrences: dict[str, list[StringFact]] = {}
        for literal in file_facts.strings:
            occurrences.setdefault(literal.value, []).append(literal)
        for group in occurrences.values():
            if len(group) > limits["string_occurrences"]:
                literal = sorted(group, key=lambda occurrence: (occurrence.line, occurrence.column))[limits["string_occurrences"]]
                violations.append(Violation(path, literal.line, literal.column,
                                            "string_occurrences", len(group), limits["string_occurrences"]))
        for number, line in enumerate(physical_lines(source), 1):
            width = physical_width(line)
            if width > limits["line_width"]:
                violations.append(Violation(path, number, limits["line_width"] + 1, "line_width", width, limits["line_width"]))
    return sorted(violations)
