import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../', import.meta.url));
process.chdir(root);
function run(command, args) {
  console.log(`> ${command} ${args.join(' ')}`);
  const result = spawnSync(command, args, {
    stdio: 'inherit',
    shell: process.platform === 'win32',
  });
  if (result.error || result.status !== 0) {
    throw new Error(`${command} failed. Check the prerequisites in docs/development.md.`);
  }
}

if (Number(process.versions.node.split('.')[0]) !== 24) throw new Error('Use Node.js 24 LTS.');
run('rustup', ['--version']);
run('cargo', ['--version']);
run('python3', ['-c', 'import sys; assert sys.version_info >= (3, 11), "Python 3.11+ required"']);
run('rustup', ['component', 'add', 'rustfmt', 'clippy']);
run('rustup', ['target', 'add', 'wasm32-unknown-unknown']);
const wasm = spawnSync('wasm-pack', ['--version'], { encoding: 'utf8' });
if (wasm.status !== 0) run('cargo', ['install', 'wasm-pack', '--version', '0.15.0', '--locked']);

// Install each lock independently. The old LSP postinstall hid two npm installs.
for (const directory of ['.', 'cddl-lsp', 'cddl-lsp/client', 'cddl-lsp/server', 'www']) {
  run('npm', ['--prefix', directory, 'ci']);
}
const python = process.platform === 'win32' ? '.venv/Scripts/python.exe' : '.venv/bin/python';
if (!existsSync(python)) run('python3', ['-m', 'venv', '.venv']);
run(python, ['-m', 'pip', 'install', '-r', 'requirements-dev.txt']);
run('wasm-pack', ['build', '--target', 'web', '--out-dir', 'pkg', '--', '--locked']);
run('npm', ['--prefix', 'cddl-lsp', 'run', 'compile']);
run('npm', ['run', 'hooks:install']);
console.log('Setup complete. Run npm run quality and npm run test:coverage.');
