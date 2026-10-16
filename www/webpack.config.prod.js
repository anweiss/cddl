const base = require('./webpack.config');

module.exports = {
  ...base,
  mode: 'production',
  devtool: false,
  stats: {
    all: false,
    assets: true,
    modules: true,
    nestedModules: false,
    reasons: false,
    source: false,
    timings: true,
    errors: true,
    errorDetails: true,
    warnings: true,
  },
  output: {
    ...base.output,
    filename: '[name].[contenthash:8].js',
    chunkFilename: '[name].[contenthash:8].js',
    clean: true,
  },
  // Monaco and the Rust validator are intentionally larger than webpack's
  // generic 244 KiB recommendation. The measured gzip budgets are enforced
  // by scripts/check-bundle.mjs instead.
  performance: {
    hints: 'error',
    maxAssetSize: 5 * 1024 * 1024,
    maxEntrypointSize: 5 * 1024 * 1024,
  },
};
