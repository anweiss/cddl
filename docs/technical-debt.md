# Tracked quality debt

These are concrete follow-ups, not waived checks.

| Area                                | Evidence                                                                                          | Completion criterion                                                                                                                                                       |
| ----------------------------------- | ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Parser/validator complexity         | `.complexity-baseline.json`, `reports/complexity.json`                                            | Extract cohesive functions, preserve all target/feature tests, and remove reduced entries from the baseline. No function may grow past its recorded ceiling.               |
| Cross-format validation duplication | `.jscpd.json`, `reports/duplication/`                                                             | Share format-independent behavior without erasing JSON/CBOR semantics; reduce the 7.3% ceiling after measured reductions.                                                  |
| Playground composition              | `www/index.js` remains large                                                                      | Extract editor/diagnostic UI responsibilities behind typed modules with behavior tests. Include extracted modules in coverage.                                             |
| Pest WASM compatibility             | Exact family pin at 2.8.8                                                                         | Upgrade together only when release web and Node WASM builds plus native/MSRV tests pass.                                                                                   |
| Monaco sanitizer override           | `www/package.json` DOMPurify override                                                             | Remove only after upstream pins a patched sanitizer and the full audit/browser suite passes.                                                                               |
| Hosted controls                     | Repository-admin settings are outside source control                                              | Maintainer verifies required checks, branch protection, private vulnerability reporting and deployment approvals. Do not infer enabled settings from workflow files.       |
| Generated CI Doctor workflow        | `actionlint` reports upstream shell quoting/style warnings in the unmodified `ci-doctor.lock.yml` | Regenerate with a fixed `gh-aw` compiler and review the generated diff. Do not hand-edit the lock or suppress the warnings. All handwritten workflows validate separately. |

When opening a follow-up issue, use the repository's bug/feature template,
include the evidence and acceptance criterion, and link it from the relevant PR.
