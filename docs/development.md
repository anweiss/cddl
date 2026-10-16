# Development

## Reproducible setup

Install Git, Node 24 LTS, Python 3.11+ (including venv), and Rust stable via
rustup. Then run `npm run setup` at the root. It installs from all npm locks,
pins Python tooling, installs wasm-pack 0.15.0 if absent, builds web and
Node/LSP WASM artifacts and installs `.githooks/pre-commit`. No secret values,
`.env` files, databases or network services are required at runtime.

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
manifests. Pest packages are held together at 2.8.8 because Pest 2.9.3 pins
a psm version that fails WASM linking. Upgrade the family only after both
release WASM builds and tests pass. Do not use `npm audit fix --force`.
Dependabot's seven-day cooldown applies to ordinary updates, not urgent
security updates.

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
