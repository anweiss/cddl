# Copilot Instructions

## Build and Test Verification

Before completing any task that modifies code, you MUST verify that all CI checks pass:

1. **Push your changes** to the branch
2. **Wait for the GitHub Actions workflow** "Build and Test" (`.github/workflows/ci.yml`) to complete
3. **Verify ALL jobs pass** - the workflow includes:
   - `minimum-version-check` (msrv check on ubuntu, macOS, windows)
   - `compilation-check` (feature combinations)
   - `wasm-compilation-check`
   - `test-suite` (on ubuntu and windows)
   - `wasm-test-suite`
   - `style-linting` (fmt and clippy)
   - `wasm-style-linting`

4. **If any job fails**, investigate the failure, fix the issue, and re-run the workflow
5. **Do NOT consider a task complete** until all CI jobs have passed successfully

## Local Verification

Before pushing, run these commands locally to catch issues early:

```bash
# Format check
cargo fmt --all -- --check

# Clippy linting (native)
cargo clippy --all

# Clippy linting (wasm)
cargo clippy --lib --target wasm32-unknown-unknown

# Compilation check (wasm)
cargo check --lib --target wasm32-unknown-unknown

# Compilation check (default features)
cargo +stable check --all --bins --examples --tests

# Compilation check (no default features)
cargo +stable check --all --bins --examples --tests --no-default-features

# Run tests
cargo test --all -- --nocapture
```

## Cross-Target and Feature Flag Awareness

This crate compiles for both native and `wasm32-unknown-unknown` targets, with conditional code gated behind `#[cfg(target_arch = "wasm32")]` and `#[cfg(not(target_arch = "wasm32"))]`. It also uses Cargo feature flags (e.g., `json`, `cbor`, `additional-controls`, `ast-span`) to conditionally compile functionality.

When modifying code:

1. **Check all `cfg` variants** — if you edit code inside a `#[cfg(not(target_arch = "wasm32"))]` block, look for a corresponding `#[cfg(target_arch = "wasm32")]` block that may need the same or analogous change, and vice versa.
2. **Check feature-gated code** — if you add, rename, or remove a type, function, or import, search for all `#[cfg(feature = "...")]` blocks that may reference it and update them too.
3. **Verify both targets compile** — always run `cargo check --lib --target wasm32-unknown-unknown` in addition to the native `cargo clippy --all` to catch wasm-only compilation errors that won't surface in native builds.
4. **Shared types must be in scope for all targets** — if a struct (e.g., `ParserError`) is used in both native and wasm code, ensure it is defined or imported in every module/target that references it.

## Rust Toolchain

- Minimum supported Rust version (MSRV): 1.81.0
- Primary toolchain: stable
