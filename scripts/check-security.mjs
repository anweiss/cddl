import { spawnSync } from 'node:child_process';
import { mkdirSync, writeFileSync } from 'node:fs';

mkdirSync('reports/security', { recursive: true });
let failed = false;
for (const directory of ['.', 'www', 'cddl-lsp', 'cddl-lsp/client', 'cddl-lsp/server']) {
  const result = spawnSync('npm', ['--prefix', directory, 'audit', '--json'], {
    encoding: 'utf8',
    shell: process.platform === 'win32',
  });
  const name = directory === '.' ? 'root' : directory.replaceAll('/', '-');
  writeFileSync(`reports/security/${name}.json`, result.stdout || '{}');
  if (result.status !== 0) {
    failed = true;
    console.error(
      `Advisories or registry failure in ${directory}; see reports/security/${name}.json`,
    );
  }
}
for (const [command, args] of [
  ['cargo', ['audit', '--json']],
  [
    'gitleaks',
    [
      'dir',
      '.',
      '--redact',
      '--no-banner',
      '--report-format',
      'json',
      '--report-path',
      'reports/security/secrets.json',
    ],
  ],
]) {
  const result = spawnSync(command, args, { encoding: 'utf8' });
  if (command === 'cargo') writeFileSync('reports/security/cargo.json', result.stdout || '{}');
  if (result.status !== 0) {
    failed = true;
    // Do not echo scanner findings or raw subprocess output that may include secrets.
    console.error(
      `${command} failed or is missing. See SECURITY.md for installation and remediation.`,
    );
  }
}
if (failed) process.exitCode = 1;
else console.log('All dependency and secret checks passed.');
