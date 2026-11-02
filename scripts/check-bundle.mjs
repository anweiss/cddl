import { readdirSync, readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { gzipSync } from 'node:zlib';

// Measured after eliminating unused language workers and duplicate WASM.
// Small headroom allows patch updates, not another full editor/language bundle.
const limits = { javascriptGzip: 1_800_000, wasmGzip: 1_100_000, totalGzip: 2_800_000 };
const assets = readdirSync('www/dist')
  .filter((file) => /\.(js|wasm)$/.test(file))
  .map((file) => {
    const content = readFileSync(`www/dist/${file}`);
    return { file, bytes: content.length, gzip: gzipSync(content).length };
  });
const sum = (suffix) =>
  assets
    .filter((asset) => asset.file.endsWith(suffix))
    .reduce((total, asset) => total + asset.gzip, 0);
const actual = {
  javascriptGzip: sum('.js'),
  wasmGzip: sum('.wasm'),
  totalGzip: assets.reduce((total, asset) => total + asset.gzip, 0),
};
mkdirSync('reports', { recursive: true });
writeFileSync('reports/bundle.json', JSON.stringify({ assets, limits, actual }, null, 2) + '\n');
if (!assets.some((asset) => asset.file.endsWith('.wasm')))
  throw new Error('Missing validator WASM.');
for (const name of Object.keys(limits)) {
  console.log(`${name}: ${actual[name]} / ${limits[name]} bytes`);
  if (!actual[name] || actual[name] > limits[name])
    throw new Error(`${name} budget exceeded or bundle missing.`);
}
