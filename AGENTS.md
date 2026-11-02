# Working on CDDL

## Repository map

- `src/`: Rust library, CLI, AST, parser and JSON/CBOR/CSV validators.
- `cddl-codegen-rust/`: Rust code generator. `cddl-derive/`: derive macros and standalone consumer.
- `cddl-lsp/client/`: VS Code extension; `cddl-lsp/server/`: stdio/IPC language server.
- `www/`: browser-only playground using the generated web WASM package.
- `tests/`: native and WASM Rust regressions. `www/tests/` and
  `cddl-lsp/server/tests/`: Vitest unit and real-WASM/protocol integration tests.

## Setup

Use Node 24 LTS, Python 3.11+ and Rust stable installed through rustup.
The supported Rust minimum is 1.88.0. From the repository root:

```sh
npm run setup
```

This installs locked dependencies, Python tools, the WASM target, wasm-pack when
missing, builds both WASM packages and installs the checked-in pre-commit hook.
See [development](docs/development.md) for troubleshooting and container setup.
There are no required credentials, environment files, databases or local services.

## Verification

```sh
npm run quality
npm run test:coverage
npm run test:stability
npm run build:playground
npm run check:bundle
npm run test:e2e
npm run check:security
cargo fmt --all -- --check
cargo clippy --all
cargo clippy --lib --target wasm32-unknown-unknown
cargo check --lib --target wasm32-unknown-unknown
cargo check --all --bins --examples --tests
cargo check --all --bins --examples --tests --no-default-features
cargo test --all
cargo test --locked --manifest-path cddl-derive/tests/consumer/Cargo.toml
```

Run `npm run build:wasm` after any Rust changes before running JavaScript tests.
Run `npm run build:lsp` after LSP changes. For interactive workflows, use
[QA instructions](docs/qa.md). CI adds platform, MSRV and feature combinations.
Do not claim hosted CI passed unless it actually ran. Never push or publish
without the user's explicit authorization.

## Boundaries

Keep web code out of the LSP and client code out of the server; use the WASM API
and LSP protocol. Rust changes must compile both natively and for WASM, with
and without feature flags. Do not edit generated `pkg/`, `out/` or `dist/`.
`Cargo.toml` owns the core version; keep `www/package.json` and README examples
in sync. The extension has independent versioning.

Follow strict TypeScript/checkJs, ESLint, rustfmt and Prettier conventions.
New functions must stay at complexity 20 or below; do not inflate the legacy
baseline or weaken coverage, duplication or bundle limits to pass a check.
Tests must use isolated synthetic documents, restore globals and timers, and
wait for observable events rather than arbitrary sleeps.

Never log document contents, filenames, paths, shared URLs, credentials or raw
exceptions. Do not send documents to external services. Preserve existing work.
Update the relevant documentation when behavior, setup or public APIs change.
See [architecture](docs/architecture.md), [security](SECURITY.md) and
[operations](docs/operations.md).
