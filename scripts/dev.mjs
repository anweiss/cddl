import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const cwd = fileURLToPath(new URL('../www', import.meta.url));
const processes = [
  spawn(process.execPath, ['node_modules/webpack/bin/webpack.js', '--watch'], {
    cwd,
    stdio: 'inherit',
  }),
  spawn('python3', ['-m', 'http.server', '8080', '--bind', '127.0.0.1', '--directory', 'dist'], {
    cwd,
    stdio: 'inherit',
  }),
];
let stopping = false;
function stop(code = 0) {
  if (stopping) return;
  stopping = true;
  for (const child of processes) child.kill();
  process.exitCode = code;
}
for (const child of processes) {
  child.on('error', (error) => {
    console.error(error.message);
    stop(1);
  });
  child.on('exit', (code) => {
    if (!stopping) stop(code || 1);
  });
}
process.on('SIGINT', () => stop());
process.on('SIGTERM', () => stop());
console.log('Watch build at http://127.0.0.1:8080. Refresh after webpack finishes.');
