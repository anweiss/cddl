import { spawnSync } from 'node:child_process';

const current = spawnSync('git', ['config', '--get', 'core.hooksPath'], { encoding: 'utf8' });
if (current.status !== 0 && current.status !== 1)
  throw new Error('Cannot read Git hook configuration.');
const configured = current.stdout.trim();
if (configured && configured !== '.githooks') {
  throw new Error(
    'An existing hooksPath is configured. Preserve it and integrate .githooks/pre-commit manually.',
  );
}
const result = spawnSync('git', ['config', '--local', 'core.hooksPath', '.githooks'], {
  stdio: 'inherit',
});
if (result.status !== 0) throw new Error('Could not configure repository hooks.');
console.log('Repository pre-commit hook installed.');
