"""Reject complexity regressions, with an explicit baseline for legacy functions."""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

import lizard

ROOT = Path(__file__).resolve().parents[1]
BASELINE = ROOT / ".complexity-baseline.json"
LIMIT = 20
SOURCE_DIRS = (
    "src/",
    "cddl-codegen-rust/src/",
    "cddl-derive/src/",
    "cddl-lsp/client/src/",
    "cddl-lsp/server/src/",
    "www/",
)


def canonical_signature(key):
    """Ignore formatter-only whitespace and trailing parameter commas."""
    return re.sub(r"\s+", "", re.sub(r",\s*(?=\))", "", key))


def analyze(ref=None):
    paths = (
        subprocess.check_output(
            ["git", "ls-tree", "-r", "--name-only", "-z", ref]
            if ref
            else [
                "git",
                "ls-files",
                "--cached",
                "--others",
                "--exclude-standard",
                "-z",
            ],
            cwd=ROOT,
        )
        .decode()
        .split("\0")
    )
    result = {}
    for path in paths:
        if not path.startswith(SOURCE_DIRS) or not path.endswith((".rs", ".js", ".ts")):
            continue
        if path.endswith((".test.ts", ".test.js", ".d.ts", "parser_tests.rs")):
            continue
        if "tests" in Path(path).parts or "test" in Path(path).parts:
            continue
        if not ref and not (ROOT / path).is_file():
            continue
        source = (
            subprocess.check_output(["git", "show", f"{ref}:{path}"], cwd=ROOT).decode()
            if ref
            else (ROOT / path).read_text()
        )
        for function in lizard.analyze_file.analyze_source_code(path, source).function_list:
            key = canonical_signature(f"{path}::{function.long_name}")
            result[key] = max(result.get(key, 0), function.cyclomatic_complexity)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--record-baseline",
        metavar="GIT_REF",
        help="One-time legacy baseline; never use to bypass a failing check.",
    )
    args = parser.parse_args()
    if args.record_baseline:
        functions = analyze(args.record_baseline)
        BASELINE.write_text(
            json.dumps(
                {
                    "reference": subprocess.check_output(
                        ["git", "rev-parse", args.record_baseline], cwd=ROOT
                    )
                    .decode()
                    .strip(),
                    "limit": LIMIT,
                    "functions": {
                        key: value for key, value in sorted(functions.items()) if value > LIMIT
                    },
                },
                indent=2,
            )
            + "\n"
        )
        return 0

    baseline = json.loads(BASELINE.read_text())
    baseline["functions"] = {
        canonical_signature(key): value for key, value in baseline["functions"].items()
    }
    functions = analyze()
    failures = [
        f"{key}: {value} > {max(LIMIT, baseline['functions'].get(key, LIMIT))}"
        for key, value in functions.items()
        if value > max(LIMIT, baseline["functions"].get(key, LIMIT))
    ]
    report = ROOT / "reports" / "complexity.json"
    report.parent.mkdir(exist_ok=True)
    report.write_text(json.dumps(functions, indent=2) + "\n")
    if failures:
        print(
            "Cyclomatic complexity regressions:\n" + "\n".join(failures),
            file=sys.stderr,
        )
        return 1
    print(
        f"Checked {len(functions)} functions: new functions <= {LIMIT}; legacy functions cannot grow."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
