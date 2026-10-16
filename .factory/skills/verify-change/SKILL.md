---
name: verify-change
description: Verify a CDDL change across Rust, WASM, the language server and browser without publishing or weakening checks.
---

# Verify a CDDL change

1. Read `AGENTS.md` and `docs/development.md`. Inspect the current Git diff and
   preserve work unrelated to the request.
2. Run `npm run setup` if tools or generated WASM packages are missing.
3. For Rust changes run the native, feature and WASM checks in `AGENTS.md`,
   then `npm run build:wasm`. For LSP changes run `npm run build:lsp`.
4. Run `npm run quality`, `npm run test:coverage` and `npm run test:stability`.
5. Build the playground, check its bundle budget and run browser tests using
   `docs/qa.md`. Use only synthetic input in logs, profiles and screenshots.
6. Fix failures at their source. Stop on missing credentials, unavailable
   runtimes or a request to publish; report the blocker. Never skip a check,
   change a baseline to hide a regression, push, deploy or publish implicitly.
7. Summarize the changed behavior and actual checks, including every check
   that could not run. Hosted CI results require a real hosted run.
