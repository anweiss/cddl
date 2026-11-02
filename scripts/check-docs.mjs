import { execFileSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';

const files = execFileSync(
  'git',
  ['ls-files', '--cached', '--others', '--exclude-standard', '-z'],
  { encoding: 'utf8' },
)
  .split('\0')
  .filter((file) => file.endsWith('.md') && existsSync(file));
const failures = [];
for (const file of files) {
  const text = readFileSync(file, 'utf8');
  for (const match of text.matchAll(/!?\[[^\]]*\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g)) {
    const href = match[1].split('#')[0];
    // Workflow prompt templates interpolate these links when an actual run exists.
    if (!href || /^(?:[a-z]+:|\/\/)/i.test(href) || /^\{[a-z_]+\}$/.test(href)) continue;
    const target = path.resolve(path.dirname(file), decodeURIComponent(href));
    if (!existsSync(target)) failures.push(`${file}: broken local link ${href}`);
  }
}
const guidance = readFileSync('AGENTS.md', 'utf8');
for (const section of ['Repository map', 'Setup', 'Verification', 'Boundaries']) {
  if (!guidance.includes(`## ${section}`)) failures.push(`AGENTS.md: missing ${section} section`);
}
const scripts = JSON.parse(readFileSync('package.json', 'utf8')).scripts;
for (const [, command] of guidance.matchAll(/npm run ([\w:-]+)/g)) {
  if (!scripts[command]) failures.push(`AGENTS.md references nonexistent npm script ${command}`);
}
if (failures.length) throw new Error(failures.join('\n'));
console.log(`Validated ${files.length} Markdown files and executable agent guidance.`);
