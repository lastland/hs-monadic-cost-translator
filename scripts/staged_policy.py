#!/usr/bin/env python3
"""Run shared checks against a disposable, complete copy of the Git index."""

from __future__ import annotations

from collections.abc import Iterator
from contextlib import contextmanager
from dataclasses import dataclass
import io
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
from typing import Final


CHECKS: Final[tuple[str, ...]] = tuple(
    "scripts/check-" + name + ".sh" for name in ("source", "shell", "hlint")
)


@dataclass(frozen=True)
class StagedFile:
    path: str
    object_id: bytes
    mode: int


@dataclass(frozen=True)
class IndexStamp:
    device: int
    inode: int
    mode: int
    size: int
    modified: int
    changed: int

    @classmethod
    def capture(cls, details: os.stat_result) -> IndexStamp:
        return cls(details.st_dev, details.st_ino, details.st_mode, details.st_size,
                   details.st_mtime_ns, details.st_ctime_ns)

    def signature(self) -> str:
        return repr((self.device, self.inode, self.mode, self.size, self.modified, self.changed))


@dataclass(frozen=True)
class FrozenIndex:
    original: Path
    stamp: IndexStamp
    content: bytes

    def verify(self) -> None:
        try:
            with self.original.open("rb") as source:
                before = IndexStamp.capture(os.fstat(source.fileno()))
                content = source.read()
                after = IndexStamp.capture(os.fstat(source.fileno()))
            current = IndexStamp.capture(self.original.stat())
        except OSError as error:
            raise RuntimeError("index changed during staged checks") from error
        if before != self.stamp or after != self.stamp or current != self.stamp or content != self.content:
            raise RuntimeError("index changed during staged checks")


def git(
    *arguments: str, cwd: Path | None = None, input: bytes | None = None,
    index: FrozenIndex | None = None,
) -> bytes:
    if index is not None:
        index.verify()
    environment: dict[str, str] = dict(
        os.environ, GIT_OPTIONAL_LOCKS="0", GIT_NO_REPLACE_OBJECTS="1",
    )
    descriptors: tuple[int, ...] = (3,) if environment.get("GIT_INDEX_FILE") == "/dev/fd/3" else ()
    result = subprocess.run(
        ["git", *arguments], cwd=cwd, env=environment, input=input,
        capture_output=True, check=False, pass_fds=descriptors,
    )
    if index is not None:
        index.verify()
    if result.returncode:
        raise RuntimeError(result.stderr.decode("utf-8", errors="replace").strip())
    return result.stdout


@contextmanager
def frozen_index() -> Iterator[FrozenIndex]:
    inherited = os.environ.get("SOURCE_POLICY_ORIGINAL_INDEX")
    original = Path(inherited) if inherited is not None else Path(os.fsdecode(git(
        "rev-parse", "--path-format=absolute", "--git-path", "index",
    ).removesuffix(b"\n")))
    descriptor = os.dup(3) if inherited is not None else os.open(original, os.O_RDONLY)
    with os.fdopen(descriptor, "rb") as source:
        source.seek(0)
        stamp = IndexStamp.capture(os.fstat(source.fileno()))
        if not stat.S_ISREG(stamp.mode):
            raise RuntimeError("unsupported Git index: not a regular file")
        if inherited is not None and stamp.signature() != os.environ.get("SOURCE_POLICY_INDEX_STAMP"):
            raise RuntimeError("index changed during staged checks")
        content = source.read()
        if IndexStamp.capture(os.fstat(source.fileno())) != stamp:
            raise RuntimeError("index changed during staged checks")
    captured = FrozenIndex(original, stamp, content)
    with tempfile.TemporaryDirectory(prefix="source-policy-index-") as temporary:
        path = Path(temporary) / "index"
        path.write_bytes(content)
        path.chmod(0o400)
        previous = os.environ.get("GIT_INDEX_FILE")
        os.environ["GIT_INDEX_FILE"] = str(path)
        try:
            captured.verify()
            yield captured
        finally:
            try:
                captured.verify()
            finally:
                if previous is None:
                    os.environ.pop("GIT_INDEX_FILE", None)
                else:
                    os.environ["GIT_INDEX_FILE"] = previous


def cached_changes(
    root: Path, intent_option: str, index: FrozenIndex, baseline: str,
) -> dict[bytes, bytes]:
    records = git(
        "diff", "--cached", "--raw", "--no-abbrev", "--no-renames", "--no-color",
        "--no-ext-diff", "--no-textconv", "-z", intent_option, baseline, "--",
        cwd=root, index=index,
    ).split(b"\0")
    if records[-1] != b"" or len(records) % 2 != 1:
        raise RuntimeError("cannot read staged change metadata")
    return dict(zip(records[1::2], records[:-1:2]))


def staged_files(root: Path, index: FrozenIndex) -> list[StagedFile]:
    # Intent-to-add entries have no staged content. Git exposes their placeholder
    # blobs via ls-files, but omits them from the tree that would be committed.
    # Compare against Git's immutable empty tree, not two independent HEAD reads.
    baseline = os.fsdecode(git(
        "hash-object", "-t", "tree", "--stdin", input=b"", cwd=root, index=index,
    ).removesuffix(b"\n"))
    visible = cached_changes(root, "--ita-visible-in-index", index, baseline)
    invisible = cached_changes(root, "--ita-invisible-in-index", index, baseline)
    intent_paths: set[bytes] = {
        path for path in visible.keys() | invisible.keys()
        if visible.get(path) != invisible.get(path)
    }
    files: list[StagedFile] = []
    for record in git("ls-files", "--stage", "-z", cwd=root, index=index).split(b"\0"):
        if not record:
            continue
        metadata, path = record.split(b"\t", 1)
        if path in intent_paths:
            continue
        mode, object_id, stage = metadata.split()
        name = path.decode("utf-8")
        if stage != b"0":
            raise RuntimeError("unmerged staged path: " + name)
        if mode == b"120000":
            raise RuntimeError("staged symlink is unsupported: " + name)
        if mode not in (b"100644", b"100755"):
            raise RuntimeError("unsupported staged mode " + os.fsdecode(mode) + ": " + name)
        if name.startswith("/") or any(
            part in ("", ".", "..") or part.casefold() == ".git"
            for part in name.split("/")
        ):
            raise RuntimeError("unsupported staged path: " + name)
        files.append(StagedFile(name, object_id, 0o755 if mode == b"100755" else 0o644))
    paths: set[str] = {entry.path for entry in files}
    for path in CHECKS:
        if path not in paths:
            raise RuntimeError("missing staged check: " + path)
    return files


def export_snapshot(
    root: Path, destination: Path, files: list[StagedFile], index: FrozenIndex,
) -> None:
    # Raw blobs avoid checkout filters, EOL conversion, export-ignore attributes,
    # and unstaged .gitattributes. No index or working-tree write is needed.
    objects = io.BytesIO(git(
        "cat-file", "--batch", cwd=root, index=index,
        input=b"".join(entry.object_id + b"\n" for entry in files),
    ))
    for entry in files:
        header = objects.readline().rstrip(b"\n").split()
        if len(header) != 3 or header[:2] != [entry.object_id, b"blob"]:
            raise RuntimeError("cannot read staged blob: " + entry.path)
        size = int(header[2])
        content = objects.read(size)
        if size < 0 or len(content) != size or objects.read(1) != b"\n":
            raise RuntimeError("incomplete staged blob: " + entry.path)
        try:
            parent = destination
            for component in entry.path.split("/")[:-1]:
                directory = parent / component
                directory.mkdir(exist_ok=True)
                if component not in os.listdir(parent):
                    raise RuntimeError("filesystem cannot represent staged path: " + entry.path)
                parent = directory
            target = destination / entry.path
            with target.open("xb") as output:
                output.write(content)
            target.chmod(entry.mode)
            if target.name not in os.listdir(target.parent):
                raise RuntimeError("filesystem cannot represent staged path: " + entry.path)
        except OSError as error:
            raise RuntimeError(
                "cannot represent staged path " + entry.path + ": " + str(error.strerror)
            ) from error
    if objects.read():
        raise RuntimeError("unexpected data after staged blobs")


def main() -> int:
    if len(sys.argv) != 1:
        raise RuntimeError("usage: sh scripts/check-staged.sh")
    with frozen_index() as index:
        root = Path(os.fsdecode(git(
            "rev-parse", "--show-toplevel", index=index,
        ).removesuffix(b"\n")))
        files = staged_files(root, index)
        with tempfile.TemporaryDirectory(prefix="source-policy-staged-") as temporary:
            snapshot = Path(temporary)
            export_snapshot(root, snapshot, files, index)
            environment: dict[str, str] = {
                name: value for name, value in os.environ.items()
                if not name.startswith(("GIT_", "SOURCE_POLICY_", "PYTHON"))
            }
            environment["PYTHONNOUSERSITE"] = "1"
            for check in CHECKS:
                result = subprocess.run(["sh", check], cwd=snapshot, env=environment, check=False)
                index.verify()
                if result.returncode:
                    return result.returncode
    print("Staged source, shell, and HLint checks passed")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print("staged-policy: " + str(error), file=sys.stderr)
        raise SystemExit(1) from None
