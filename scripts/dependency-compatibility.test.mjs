import assert from 'node:assert/strict';
import { mkdtemp, mkdir, rm, writeFile } from 'node:fs/promises';
import Module, { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';

const rootRequire = createRequire(new URL('../package.json', import.meta.url));
const extensionRequire = createRequire(new URL('../cddl-lsp/package.json', import.meta.url));
const clientRequire = createRequire(new URL('../cddl-lsp/client/package.json', import.meta.url));

test('patched language client validates and matches globs with minimatch 11', async () => {
  const { applyPatch } = clientRequire('./scripts/patch-languageclient.cjs');
  assert.doesNotThrow(() => applyPatch({ check: true }));

  const vscodeStub = {
    Uri: { parse: () => assert.fail('relative patterns are not exercised') },
    FileType: { File: 1, Directory: 2 },
    workspace: { onDidCreateFiles: () => ({ dispose() {} }) },
  };
  const stubPath = join(tmpdir(), 'cddl-vscode-stub.cjs');
  const originalResolve = Module._resolveFilename;
  Module._resolveFilename = function (request, ...rest) {
    return request === 'vscode' ? stubPath : originalResolve.call(this, request, ...rest);
  };
  Module._cache[stubPath] = { id: stubPath, filename: stubPath, loaded: true, exports: vscodeStub };
  try {
    const uri = (fsPath) => ({ fsPath, scheme: 'file', toString: () => fsPath });
    const { matchGlobPattern } = clientRequire(
      'vscode-languageclient/$test/common/utils/globPattern',
    );
    assert.equal(matchGlobPattern('**/*.cddl', uri('/work/nested/a.cddl')), true);
    assert.equal(matchGlobPattern('**/*.cddl', uri('/work/nested/a.json')), false);
    assert.equal(matchGlobPattern('', uri('/work/a.cddl')), false);

    const { DidCreateFilesFeature } = clientRequire(
      'vscode-languageclient/$test/common/fileOperations',
    );
    const feature = new DidCreateFilesFeature({
      code2ProtocolConverter: { asDidCreateFilesParams: (event) => event },
    });
    feature.register({
      id: 'synthetic',
      registerOptions: { filters: [{ scheme: 'file', pattern: { glob: '**/*.{cddl,json}' } }] },
    });
    const files = [uri('/work/a.cddl'), uri('/work/b.txt'), uri('/work/c/d.json')];
    const result = await feature.filter({ files }, (item) => item);
    assert.deepEqual(
      result.files.map((item) => item.fsPath),
      ['/work/a.cddl', '/work/c/d.json'],
    );
    assert.throws(
      () =>
        feature.register({
          id: 'invalid',
          registerOptions: { filters: [{ pattern: { glob: '' } }] },
        }),
      /Invalid pattern/,
    );
    feature.clear();
  } finally {
    Module._resolveFilename = originalResolve;
    delete Module._cache[stubPath];
  }
});

test('overridden consumers retain their minimatch matching APIs', () => {
  for (const [require, name] of [
    [rootRequire, 'eslint'],
    [rootRequire, '@eslint/config-array'],
    [rootRequire, '@typescript-eslint/typescript-estree'],
    [extensionRequire, '@vscode/vsce'],
    [clientRequire, 'mocha'],
    [clientRequire, 'vscode-languageclient'],
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
