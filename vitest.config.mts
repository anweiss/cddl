import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['www/tests/**/*.test.js', 'cddl-lsp/server/tests/**/*.test.ts'],
    environment: 'node',
    isolate: true,
    maxWorkers: 2,
    retry: 0,
    testTimeout: 15_000,
    reporters: ['default', 'json', 'junit'],
    outputFile: {
      json: 'reports/tests.json',
      junit: 'reports/junit.xml',
    },
    coverage: {
      provider: 'v8',
      include: ['www/src/*.js'],
      reporter: ['text', 'json-summary', 'lcov', 'html'],
      reportsDirectory: 'reports/coverage',
      thresholds: { statements: 75, lines: 75, functions: 75, branches: 65 },
    },
  },
});
