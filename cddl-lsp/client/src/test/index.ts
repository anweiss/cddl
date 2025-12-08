import * as path from "path";
import Mocha from "mocha";
import { glob } from "glob";

export async function run(): Promise<void> {
  // Create the mocha test
  const mocha = new Mocha({
    ui: "tdd",
  });
  mocha.options.color = true;
  mocha.timeout(100000);

  const testsRoot = __dirname;

  try {
    // Use the modern promise-based glob API
    const files = await glob("**/*.test.js", { cwd: testsRoot });

    // Add files to the test suite
    files.forEach((f) => mocha.addFile(path.resolve(testsRoot, f)));

    // Run the mocha test
    return new Promise<void>((resolve, reject) => {
      mocha.run((failures) => {
        if (failures > 0) {
          reject(new Error(`${failures} tests failed.`));
        } else {
          resolve();
        }
      });
    });
  } catch (err) {
    console.error(err);
    throw err;
  }
}
