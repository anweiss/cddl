import * as path from "path";
import { mkdtemp, rm } from "fs/promises";
import { tmpdir } from "os";

import { downloadAndUnzipVSCode, runTests } from "@vscode/test-electron";

// Resolving and downloading VS Code hits update.code.visualstudio.com, which
// intermittently fails in CI (several jobs download it concurrently). Retry a
// few times before giving up.
const DOWNLOAD_ATTEMPTS = 3;
const RETRY_DELAY_MS = 5000;

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function downloadVSCodeWithRetry(): Promise<string> {
  let lastError: unknown;

  for (let attempt = 1; attempt <= DOWNLOAD_ATTEMPTS; attempt++) {
    try {
      return await downloadAndUnzipVSCode();
    } catch (err) {
      lastError = err;
      console.error(`Failed to download VS Code (attempt ${attempt} of ${DOWNLOAD_ATTEMPTS})`, err);

      if (attempt < DOWNLOAD_ATTEMPTS) {
        await delay(RETRY_DELAY_MS * attempt);
      }
    }
  }

  throw lastError;
}

async function main() {
  const userDataDir = await mkdtemp(path.join(tmpdir(), "cddl-vscode-test-"));
  try {
    // The folder containing the Extension Manifest package.json
    // Passed to `--extensionDevelopmentPath`
    const extensionDevelopmentPath = path.resolve(__dirname, "../../../");

    // The path to test runner
    // Passed to --extensionTestsPath
    const extensionTestsPath = path.resolve(__dirname, "./index");

    // Download VS Code, unzip it and run the integration test
    const vscodeExecutablePath = await downloadVSCodeWithRetry();
    await runTests({
      vscodeExecutablePath,
      extensionDevelopmentPath,
      extensionTestsPath,
      launchArgs: [
        `--user-data-dir=${userDataDir}`,
        `--extensions-dir=${path.join(userDataDir, "extensions")}`,
        "--disable-extensions",
        "--skip-welcome",
        "--skip-release-notes",
      ],
    });
  } catch (err) {
    console.error("Failed to run tests", err);
    process.exitCode = 1;
  } finally {
    await rm(userDataDir, { recursive: true, force: true });
  }
}

main();
