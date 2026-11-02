import { mkdirSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { performance } from 'node:perf_hooks';

mkdirSync('reports', { recursive: true });
const wasm = createRequire(import.meta.url)('../cddl-lsp/server/pkg/cddl.js');
const schema = Array.from({ length: 100 }, (_, i) => `rule${i} = { value: uint, name: tstr }`).join(
  '\n',
);
const timings = [];
for (let i = 0; i < 250; i++) {
  const start = performance.now();
  wasm.cddl_from_str(schema);
  timings.push(performance.now() - start);
}
timings.sort((a, b) => a - b);
const result = {
  workload: 'parse 100 synthetic map rules',
  iterations: timings.length,
  medianMs: timings[125],
  p95Ms: timings[237],
};
writeFileSync('reports/profile.json', JSON.stringify(result, null, 2) + '\n');
console.log(result);
