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

# Clippy linting
cargo clippy --all

# Run tests
cargo test --all -- --nocapture
```

## Rust Toolchain

- Minimum supported Rust version (MSRV): 1.81.0
- Primary toolchain: stable
