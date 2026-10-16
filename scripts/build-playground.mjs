import { spawnSync } from 'node:child_process';
import { mkdirSync } from 'node:fs';

mkdirSync('reports', { recursive: true });
const start = performance.now();
const result = spawnSync(
  'npm',
  ['--prefix', 'www', 'run', 'build', '--', '--profile', '--json=../reports/webpack-stats.json'],
  { stdio: 'inherit', shell: process.platform === 'win32' },
);
console.log(`Playground build: ${((performance.now() - start) / 1000).toFixed(2)} seconds.`);
process.exitCode = result.status || (result.error ? 1 : 0);
