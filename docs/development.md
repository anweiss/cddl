# Development

## Reproducible setup

Install Git, Node 24 LTS, Python 3.11+ (including venv), and Rust stable via
rustup. Then run `npm run setup` at the root. It installs from all npm locks,
pins Python tooling, installs wasm-pack 0.15.0 if absent, builds web and
Node/LSP WASM artifacts and installs `.githooks/pre-commit`. No secret values,
`.env` files, databases or network services are required at runtime.

The pinned Pest 2.9.5 family does not enable its stack guard on WASM by default.
Standard macOS WASM builds therefore need no LLVM archiver override. CI links
both default and LSP release WASM builds on macOS without that workaround.

The first build downloads Rust crates and may take several minutes. Package
registries must be reachable during setup. Never commit credentials to solve
registry access failures. Keep `cargo`, `rustup` and `wasm-pack` on `PATH`.
For the full JS/Python quality toolchain on Windows, use WSL or the dev
container. Native Rust and extension tests still run on Windows in CI.
The checked-in hook and Python tooling commands use POSIX paths.

Open the repository in a Dev Container to provision Rust, Node and Python,
then let `postCreateCommand` run setup. Docker must be running. The container
does not need privileged mode or a database.
Named volumes isolate Python, npm and Cargo build artifacts from host-platform
dependencies. Setup only fixes ownership inside those volumes; it does not
change ownership of source files.

## Commands

| Goal                                   | Command                          |
| -------------------------------------- | -------------------------------- |
| Static gates                           | `npm run quality`                |
| Format JS, TS, JSON and docs           | `npm run format`                 |
| Format Rust                            | `cargo fmt --all`                |
| Native CLI                             | `cargo run --bin cddl -- --help` |
| Rust API docs                          | `cargo doc --all --no-deps`      |
| Web and LSP WASM                       | `npm run build:wasm`             |
| Extension and server                   | `npm run build:lsp`              |
| Web production build and webpack stats | `npm run build:playground`       |
| Web development server                 | `npm run dev`                    |
| Coverage and JUnit/timing results      | `npm run test:coverage`          |
| Repeat tests without retrying failures | `npm run test:stability`         |
| Browser QA                             | `npm run test:e2e`               |
| Dependency advisories                  | `npm run check:security`         |
| CPU profile                            | `npm run profile`                |

`reports/` holds generated results, not source files. CI uploads them even
when a check fails. Unit tests run without a server. Browser tests start an
isolated loopback static server and close it afterwards.

## Dependency and feature changes

Commit npm and Cargo lockfiles. Use `npm run check:versions` when editing
manifests. Keep the Pest family pinned together at 2.9.5. Earlier versions
pulled psm into WASM builds, requiring an LLVM archiver workaround on macOS.
Validate both release WASM packages, runtime tests and native/MSRV builds
before upgrading the family again.
With `std`, Pest 2.9.5 enables its stack-headroom guard on selected GNU/Linux,
macOS and MSVC Windows targets, but not on WASM or musl by default. CDDL's
separate `native-stack` growth still wraps supported library parse calls on
musl as well, including the entire ABNF grammar-to-VM validation path.
`tests/small_stack.rs` and the ABNF unit test cover 64 KiB threads.
The `std` feature enables `native-stack` automatically. Native applications
using `default-features = false` must enable `native-stack` if another dependency
enables `pest/std` through Cargo feature unification and they need small-stack
parsing. This opt-in uses the host standard library through `stacker` without
enabling the other `cddl/std` functionality; leave it disabled on bare-metal
no_std targets. It has no effect on WASM.
CI tests `_build-parser,native-stack,pest/std` without default features and
continues checking the bare-metal no_std build separately. Native amd64/arm64
musl jobs link the release CLI and execute the small-stack regressions.
Callers that use `pest_parser::CddlParser` directly get Pest's behavior.
Stack checks and growth are not portable stack-overflow or resource guarantees.
Do not use `npm audit fix --force`.
Dependabot's seven-day cooldown applies to ordinary updates, not urgent
security updates.

The root, extension and client npm projects override minimatch to 11.0.3 for
GHSA-g3hv-75wg-m249, and the extension and client use glob 14.0.3 for
GHSA-3g3m-2fjr-6pw4. Run `npm run test:dependencies` after changing these
overrides; it exercises the matching and discovery APIs the affected tools use.

minimatch 11 removed `makeRe()`, which vscode-languageclient 10.1.2 and 11.0.0
still call to reject empty patterns. The client's `postinstall` script,
`cddl-lsp/client/scripts/patch-languageclient.cjs`, replaces those two checks
with the equivalent `matcher.set.length === 0` test. It refuses unknown
language client versions or unexpected source. The client webpack build and
`npm --prefix cddl-lsp run compile` fail if the patch is missing, for example
after installing with `--ignore-scripts`. Remove the script, hook and checks
once a vscode-languageclient release depends on minimatch 11.0.1 or newer.

Cargo features are the project's feature flags. `Cargo.toml` defines
`json`, `cbor`, `additional-controls`, `ast-span`, `ast-comments`, `lsp` and
the parser-generation options. `lsp` builds a parser-only WASM API, not the
instance validators. Tests deliberately load the web WASM package for
JSON/CBOR validation. See [architecture](architecture.md) for data flow.

## Quality debt

The initial complexity baseline records pre-existing functions above 20
against the recorded Git revision; their complexity cannot grow. Duplication
measures 7.21% at the original revision with the same 10-line/100-token settings,
with a 7.3% ceiling. These are explicit legacy ceilings, not
claims that the old parser is simple or duplication-free. Reduce them as code
is refactored; never rebaseline a regression. Coverage covers all `www/src`
modules; browser composition in `www/index.js` is exercised by Playwright.
The Rust suite and LSP protocol tests are separate and must also pass.
