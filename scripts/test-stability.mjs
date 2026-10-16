import { spawnSync } from 'node:child_process';
import { mkdirSync, readFileSync } from 'node:fs';

mkdirSync('reports', { recursive: true });
let expected;
for (let run = 1; run <= 3; run++) {
  const file = `reports/stability-${run}.json`;
  const result = spawnSync(
    process.execPath,
    ['node_modules/vitest/vitest.mjs', 'run', '--reporter=json', `--outputFile=${file}`],
    { stdio: 'inherit' },
  );
  if (result.status !== 0) process.exit(result.status || 1);
  const report = JSON.parse(readFileSync(file, 'utf8'));
  const names = report.testResults
    .flatMap((suite) => suite.assertionResults.map((test) => test.fullName))
    .sort();
  if (!names.length || (expected && JSON.stringify(expected) !== JSON.stringify(names))) {
    throw new Error('Test discovery changed between runs or no tests ran.');
  }
  expected = names;
}
console.log(`All ${expected.length} tests passed three fresh runs without retries.`);
