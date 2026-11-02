# Build, release and incident operations

## Before release

Run the full [verification matrix](../AGENTS.md#verification). Review coverage,
test timing JSON, complexity, duplication, bundle and security reports. Build
Rustdoc with `cargo doc --all --no-deps`. Existing release workflows perform
package dry runs and create draft releases; publishing a draft is a separate,
authorized maintainer action. Keep the core/playground versions synchronized.
Inspect staged changes for credentials before committing, and do not push,
publish or merge without authorization.

CI's quality workflow uploads test timing, coverage, webpack build timing/module
stats, size and analysis reports even on failure. Compare them to the last
successful run when investigating a slowdown. The early static phase should
fail before expensive browser tests. The existing cross-platform Rust CI
remains authoritative for MSRV and feature compatibility.

## Smoke checks and rollback

For a staged static deployment, confirm the HTML, hashed JavaScript, editor and
JSON workers and WASM all load. Validate one valid and one invalid synthetic
instance. Check browser console for sanitized load failures. Never promote
assets from a failing build.

If a release regresses, stop promotion. Rebuild the last known-good **tag** in a
fresh worktree, run its checks and deploy its complete asset set through the
existing approved publishing workflow. Do not mix old HTML with new hashed
assets or overwrite registry versions. Publish a forward-fix version for
immutable crates/npm releases. Marketplace rollback requires a new extension
version. Hosted deployment rollback/approval settings require maintainers;
there is no claim of automatic production rollback or canary rollout.

## Troubleshooting

| Symptom                               | Investigation and recovery                                                                                                                                               |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| WASM load failure                     | Check the HTTP status and WASM MIME type; rebuild both WASM packages, then webpack. Do not copy stale generated glue.                                                    |
| `rust_psm_stack_pointer` linker error | Check that the Pest family remains at the compatible locked version; see [development](development.md).                                                                  |
| Missing test validator exports        | Use the web WASM package, not the `lsp` parser-only build.                                                                                                               |
| Formatter falls back                  | Reproduce with a synthetic schema; inspect `formatter.wasm_failed` in LSP logs and run the protocol/native tests.                                                        |
| Bundle budget exceeded                | Inspect `reports/webpack-stats.json` for added languages, workers or duplicate WASM; remove accidental dependencies rather than raising budgets.                         |
| Dependency advisory                   | Inspect local redacted reports, patch the affected chain, rebuild and rerun the full security command. Escalate if no compatible fix exists.                             |
| Flaky or slow test                    | Compare JSON test durations and stability reports. Reproduce in isolation; fix leaked state, listeners or timing assumptions. Do not add retries or quarantine silently. |

## Profiling

`npm run profile` parses 100 synthetic CDDL rules 250 times through the Node WASM
API. It writes median/p95 latency and a Node CPU profile in `reports/`. Inspect
the `.cpuprofile` with browser DevTools. This is a reproducible diagnostic
workload, not an end-user SLO or a hardware-independent performance assertion.
For native CPU investigation use `cargo build --profile profiling`; the
profile retains debug information with release optimization.

There is no hosted service to page, distributed trace backend or production
health endpoint. Local sanitized errors, reproducible tests and maintainer
issue triage are the supported error-to-fix path. External alerting, product
analytics and error-upload systems are deliberately not fabricated.
