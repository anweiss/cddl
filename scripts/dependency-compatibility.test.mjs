import assert from 'node:assert/strict';
import { mkdtemp, mkdir, rm, writeFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';

const rootRequire = createRequire(new URL('../package.json', import.meta.url));
const extensionRequire = createRequire(new URL('../cddl-lsp/package.json', import.meta.url));
const clientRequire = createRequire(new URL('../cddl-lsp/client/package.json', import.meta.url));

test('language client keeps the legacy makeRe API until upstream migrates', () => {
  const consumerRequire = createRequire(clientRequire.resolve('vscode-languageclient'));
  const { Minimatch } = consumerRequire('minimatch');
  assert.equal(typeof new Minimatch('**/*.cddl').makeRe, 'function');
});

test('overridden consumers retain their minimatch matching APIs', () => {
  for (const [require, name] of [
    [rootRequire, 'eslint'],
    [rootRequire, '@eslint/config-array'],
    [rootRequire, '@typescript-eslint/typescript-estree'],
    [extensionRequire, '@vscode/vsce'],
    [clientRequire, 'mocha'],
  ]) {
    const consumerRequire = createRequire(require.resolve(name));
    const { minimatch, Minimatch } = consumerRequire('minimatch');
    assert.equal(consumerRequire('minimatch/package.json').version, '11.0.3');
    assert.equal(minimatch('nested/example.cddl', '**/*.{cddl,json}'), true);
    assert.equal(minimatch('.hidden/example.cddl', '**/*.cddl', { dot: true }), true);
    assert.equal(new Minimatch('**/*.cddl').match('nested', true), true);
    assert.equal(new Minimatch('**/*.cddl').match('nested/example.json'), false);
  }
});

test('ESLint still discovers files and parses TypeScript with the override', async () => {
  const { ESLint } = rootRequire('eslint');
  const eslint = new ESLint();
  const results = await eslint.lintText('const value: number = 1;\nvoid value;\n', {
    filePath: 'cddl-lsp/client/src/synthetic.ts',
  });
  assert.equal(results.length, 1);
  assert.equal(results[0].errorCount, 0);
  assert.equal(results[0].warningCount, 0);
});

test('glob discovery, Mocha discovery, and rimraf glob deletion remain compatible', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'cddl-dependencies-'));
  try {
    await mkdir(join(directory, 'nested'));
    await writeFile(join(directory, 'nested', 'sample.test.js'), '');
    await writeFile(join(directory, 'nested', 'ignored.test.js'), '');
    await writeFile(join(directory, 'nested', '.hidden.test.js'), '');
    const { glob, globSync } = clientRequire('glob');
    const options = { cwd: directory, ignore: '**/ignored.*' };
    assert.deepEqual(await glob('**/*.test.js', options), ['nested/sample.test.js']);
    assert.deepEqual(globSync('**/*.test.js', options), ['nested/sample.test.js']);
    const collectFiles = clientRequire('mocha/lib/cli/collect-files.cjs');
    const files = collectFiles({
      spec: [join(directory, '**/*.test.js')],
      ignore: [join(directory, '**/ignored.*')],
      extension: ['js'],
      file: [],
    });
    assert.deepEqual(files.files, [join(directory, 'nested', 'sample.test.js')]);
    const { rimraf } = extensionRequire('rimraf');
    await rimraf(join(directory, '**/*.test.js'), { glob: { dot: true } });
    assert.deepEqual(await glob('**/*.test.js', { cwd: directory, dot: true }), []);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
