"""Enforce consistent dependency requirements across every checked-in build module."""

import json
import re
import subprocess
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def main():
    paths = subprocess.check_output(["git", "ls-files", "-z"], cwd=ROOT).decode().split("\0")
    paths = sorted(set(paths + ["package.json"]))
    requirements = {}
    errors = []
    packages = {}
    cargo = []
    for path in paths:
        file = ROOT / path
        if file.name == "Cargo.toml":
            data = tomllib.loads(file.read_text())
            if "package" in data:
                packages[file.parent.resolve()] = data["package"]["version"]
            cargo.append((file, data))
        elif file.name == "package.json" and file.is_file():
            data = json.loads(file.read_text())
            for section in ("dependencies", "devDependencies"):
                for name, version in data.get(section, {}).items():
                    if version.startswith(("file:", "workspace:")):
                        continue
                    requirements.setdefault(("npm", name), []).append(
                        (path, re.sub(r"^[~^=]", "", version))
                    )
    for file, data in cargo:
        for section in ("dependencies", "dev-dependencies", "build-dependencies"):
            for name, spec in data.get(section, {}).items():
                version = spec if isinstance(spec, str) else spec.get("version")
                if not version:
                    continue
                path = file.relative_to(ROOT).as_posix()
                if isinstance(spec, dict) and "path" in spec:
                    target = (file.parent / spec["path"]).resolve()
                    if target in packages and version.lstrip("=") != packages[target]:
                        errors.append(
                            f"{path}: {name} requires {version}, but local package is {packages[target]}"
                        )
                else:
                    requirements.setdefault(("cargo", name), []).append((path, version.lstrip("=")))
    for (ecosystem, name), declarations in requirements.items():
        if len({version for _, version in declarations}) > 1:
            errors.append(
                f"{ecosystem} {name}: "
                + ", ".join(f"{path}={version}" for path, version in declarations)
            )
    root = tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"]
    web = json.loads((ROOT / "www/package.json").read_text())["version"]
    if root != web:
        errors.append(f"Core/playground release versions differ: {root} != {web}")
    if errors:
        print("Dependency version drift:\n" + "\n".join(errors), file=sys.stderr)
        return 1
    print("Dependency requirements and local package versions are consistent.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
