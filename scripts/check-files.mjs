import { execFileSync } from 'node:child_process';
import { statSync } from 'node:fs';

const limit = 1024 * 1024;
const files = execFileSync(
  'git',
  ['ls-files', '--cached', '--others', '--exclude-standard', '-z'],
  {
    encoding: 'utf8',
  },
)
  .split('\0')
  .filter(Boolean);
const oversized = [...new Set(files)].filter((file) => {
  try {
    return statSync(file).isFile() && statSync(file).size > limit;
  } catch (error) {
    if (error.code === 'ENOENT') return false;
    throw error;
  }
});
if (oversized.length) {
  console.error(`Files exceed the 1 MiB source limit:\n${oversized.join('\n')}`);
  process.exitCode = 1;
} else {
  console.log(`Checked ${files.length} files against the 1 MiB limit.`);
}
