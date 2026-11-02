'use strict';

// minimatch 11.0.1+ fixes GHSA-g3hv-75wg-m249 but removes Minimatch#makeRe().
// vscode-languageclient 10.1.2 and 11.0.0 call makeRe() only to reject patterns
// that compile to nothing; an empty match set is the minimatch 11 equivalent.
// Remove this patch, its postinstall hook and the build checks once a
// vscode-languageclient release depends on minimatch >=11.0.1.
const fs = require('node:fs');
const path = require('node:path');

const SUPPORTED_VERSIONS = new Set(['10.1.2', '11.0.0']);
const ORIGINAL = 'if (!matcher.makeRe()) {';
const PATCHED = 'if (matcher.set.length === 0) {';
const FILES = ['lib/common/fileOperations.js', 'lib/common/utils/globPattern.js'];

function count(source, text) {
  return source.split(text).length - 1;
}

function applyPatch({ check = false } = {}) {
  const root = path.join(__dirname, '..', 'node_modules', 'vscode-languageclient');
  const { version } = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
  if (!SUPPORTED_VERSIONS.has(version)) {
    throw new Error(
      `vscode-languageclient ${version} is not covered by the minimatch 11 patch. ` +
        'Review cddl-lsp/client/scripts/patch-languageclient.cjs.',
    );
  }
  for (const file of FILES) {
    const target = path.join(root, file);
    const source = fs.readFileSync(target, 'utf8');
    const original = count(source, ORIGINAL);
    const patched = count(source, PATCHED);
    if (original === 0 && patched === 1) continue;
    if (original !== 1 || patched !== 0) {
      throw new Error(`vscode-languageclient ${file} does not match the expected source.`);
    }
    if (check) {
      throw new Error(
        `vscode-languageclient ${file} is not patched for minimatch 11. ` +
          'Run npm --prefix cddl-lsp/client ci without --ignore-scripts.',
      );
    }
    fs.writeFileSync(target, source.replace(ORIGINAL, PATCHED));
  }
}

module.exports = { applyPatch };

if (require.main === module) {
  try {
    applyPatch({ check: process.argv.includes('--check') });
  } catch (error) {
    console.error(error instanceof Error ? error.message : 'Language client patch failed.');
    process.exitCode = 1;
  }
}
