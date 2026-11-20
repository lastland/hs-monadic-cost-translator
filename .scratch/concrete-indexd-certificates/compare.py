"""Compare the pinned public checker's complete frozen-081 diagnostics."""

from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import sys


def diagnostics(path: Path) -> Counter[str]:
    reports: list[str] = []
    for line in path.read_text().splitlines():
        if "/Generated.hs:" in line and "unapproved" in line:
            reports.append(line.split("/Generated.hs:", 1)[1])
        elif reports and line and line != ")":
            reports[-1] += "\n" + line
    return Counter(re.sub(r"_a[0-9A-Za-z]+", "_UNIQUE", row) for row in reports)


def main() -> None:
    before_path, after_path, generated = map(Path, sys.argv[1:])
    before: Counter[str] = diagnostics(before_path)
    after: Counter[str] = diagnostics(after_path)
    removed: Counter[str] = before - after
    added: Counter[str] = after - before
    assert sum(before.values()) == 467
    assert sum(after.values()) == 449
    assert sum(removed.values()) == 18 and not added, (removed, added)
    assert all(row.startswith(("895:", "897:")) for row in removed)
    assert sum(n for row, n in removed.items() if "GHC.Magic.dataToTag#" in row) == 4
    print(json.dumps({
        "generated_sha256": hashlib.sha256(generated.read_bytes()).hexdigest(),
        "before": sum(before.values()),
        "after": sum(after.values()),
        "removed": dict(removed),
        "added": dict(added),
    }, indent=2))


if __name__ == "__main__":
    main()
