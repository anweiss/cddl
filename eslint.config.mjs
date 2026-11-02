import js from '@eslint/js';
import tsParser from '@typescript-eslint/parser';
import tsPlugin from '@typescript-eslint/eslint-plugin';
import globals from 'globals';

export default [
  {
    ignores: [
      '**/node_modules/**',
      '**/target/**',
      '**/pkg/**',
      '**/out/**',
      '**/dist/**',
      '**/coverage/**',
      '**/.vscode-test/**',
      '**/*.tsbuildinfo',
      'reports/**',
      'playwright-report/**',
      'test-results/**',
      '.venv/**',
    ],
  },
  js.configs.recommended,
  {
    files: ['**/*.{js,mjs,cjs,ts,mts}'],
    languageOptions: {
      globals: { ...globals.node },
    },
    rules: {
      'no-unused-vars': ['error', { argsIgnorePattern: '^_', caughtErrors: 'none' }],
      camelcase: ['error', { properties: 'never', ignoreDestructuring: true }],
      'no-restricted-imports': [
        'error',
        {
          patterns: [
            {
              group: ['**/cddl-lsp/**', '**/www/**'],
              message:
                'Applications must communicate through the Rust/WASM API, not source imports.',
            },
          ],
        },
      ],
    },
  },
  {
    files: ['www/{index,bootstrap}.js', 'www/src/**/*.js', 'www/tests/**/*.js'],
    languageOptions: {
      sourceType: 'module',
      globals: { ...globals.browser },
    },
    rules: {
      'no-restricted-imports': [
        'error',
        {
          patterns: [
            {
              group: ['**/cddl-lsp/**', '**/src/**/*.rs'],
              message: 'Browser code must use the generated WASM API.',
            },
          ],
        },
      ],
    },
  },
  {
    files: ['**/*.{ts,mts}'],
    languageOptions: { parser: tsParser },
    plugins: { '@typescript-eslint': tsPlugin },
    rules: {
      'no-undef': 'off',
      'no-unused-vars': 'off',
      '@typescript-eslint/no-unused-vars': [
        'error',
        { argsIgnorePattern: '^_', caughtErrors: 'none' },
      ],
      '@typescript-eslint/no-duplicate-enum-values': 'error',
      '@typescript-eslint/no-non-null-asserted-optional-chain': 'error',
      '@typescript-eslint/naming-convention': [
        'error',
        { selector: 'typeLike', format: ['PascalCase'] },
      ],
      'no-restricted-imports': [
        'error',
        {
          patterns: [
            {
              group: ['**/www/**'],
              message: 'The language server must not depend on browser application code.',
            },
            {
              group: ['**/client/src/**', '**/server/src/**'],
              message: 'Client/server boundaries must use the language-server protocol.',
            },
          ],
        },
      ],
    },
  },
  {
    files: ['scripts/*.mjs', 'e2e/**/*.ts'],
    rules: { complexity: ['error', 20] },
  },
];
