# Security and privacy

Report vulnerabilities using GitHub's private vulnerability reporting for this
repository, if enabled. Otherwise contact the maintainer through the contact
listed in `Cargo.toml` to arrange a private report. Do not put sensitive payloads,
documents, tokens or exploit details in a public issue. The latest release is
the supported security-fix target; older versions may require an upgrade.

## Local checks

Install `cargo-audit` 0.22.2 or newer and Gitleaks 8.30.1 or newer. On macOS,
`brew install cargo-audit gitleaks` provides both. Then run:

```sh
npm run check:security
npm run test:dast
```

The first command audits **all five** npm locks and the Rust lock, and scans
current source files (including new files) for secrets. It fails on advisories,
missing scanners or registry failures. Reports are redacted and kept in ignored
`reports/security/`. Review them locally before sharing. There are no blanket
advisory exemptions. Generated artifacts, caches and dependencies are excluded
from the source secret scan; their dependencies are audited separately.

`test:dast` runs targeted browser security regressions against an isolated local
production build: hostile shared fragments, HTML injection and unexpected
network requests. It is not a full penetration test or a substitute for review.
CodeQL and container scanning remain in the existing workflows.

Dependabot proposes dependency updates with a seven-day cooldown for routine
updates. Security fixes are not delayed. Review lockfile changes, run all target
builds, and avoid forced major-version audit fixes. A transitive DOMPurify
override supplies a patched release while Monaco still pins an older version.
The vulnerable development proxy chain was removed in favor of a loopback-only
static server and webpack watch builds.

## Data handling

Schemas and instances may contain personal or confidential information. The
playground processes them locally and has no analytics, error-upload SDK or
backend. Browser localStorage can retain editor state; use the editor's reset
controls or clear this site's storage to remove it. Share links explicitly
contain document data in the fragment. Anyone who receives a link can read it,
and browser history, clipboard managers or screenshots can retain it. Never
share confidential examples.

Operational browser logs contain fixed event names and error categories, not
exception messages, source text, filenames or shared URLs. The LSP formatter
logs a sanitized fallback event over the LSP logging channel. CLI diagnostics
intentionally display local validation errors to the user; do not upload them
without redaction. Profiles and test artifacts must use synthetic data only.

No application secrets or `.env` values are required. Package publication and
deployment credentials belong in the CI provider's encrypted secret store, not
source files. GitHub branch protection, secret protection, private reporting,
required checks and deployment approvals require repository-administrator
configuration; checked-in workflows cannot prove those settings are enabled.
