# Interactive and automated QA

Run `npm run setup`, then `npm exec playwright install chromium` once to install
the test browser. `npm run build:playground` creates the production assets.
`npm run test:e2e` starts a loopback-only static server at port 4173 and runs
Chromium against that build in a fresh context for every test. It never uses
a real account or document. No automatic retries hide failures.

## Playground

For manual checks run `npm run dev` and open `http://127.0.0.1:8080`.
Webpack watches sources; refresh after a rebuild. Use synthetic documents.

1. Enter `root = { count: uint }`. Confirm the outline and ready status appear.
2. Open the instance pane; validate `{"count":1}` successfully, then confirm
   `{"count":-1}` and malformed JSON fail with useful diagnostics.
3. Generate a sample. Confirm it is checked by the WASM validator rather than
   merely displayed as a successful guess.
4. Enter `root = {` and confirm a schema diagnostic. Correct it and confirm
   the diagnostic clears.
5. Import a synthetic `.cddl`, `.json` and `.cbor` file, download the schema,
   and round-trip an explicitly created share link in a new context.
6. Open shortcuts using `?`, check Tab focus stays in the dialog and Escape
   returns focus. Check outline keyboard navigation and dark/light themes.

## Language server / VS Code

The extension requires VS Code 1.91 or newer, matching the language-client
runtime minimum. Extension-host tests use a fresh profile for every run.

`npm run build:lsp` prepares the real server and client. Run the **Launch
Client** debug configuration in `cddl-lsp/.vscode/launch.json`, or run
`npm --prefix cddl-lsp test` for the extension-host suite. The automated stdio
protocol tests in the root Vitest suite do not require a desktop VS Code.
Open a synthetic `.cddl`, check diagnostics clear after corrections, request
completion, hover and go-to-definition, then format the document.

## Failure evidence

Vitest writes JUnit and JSON results (including per-test duration) to `reports/`.
`npm run test:stability` repeats the discovered suite three times without
retries and checks discovery stays stable. Playwright writes its HTML report
and failure traces/screenshots to ignored directories. Only attach synthetic
test artifacts to an issue. Record reproduction steps, affected target,
feature flags and the exact failing command; do not commit generated reports.
