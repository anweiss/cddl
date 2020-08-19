//@ts-check

'use strict';

const withDefaults = require('../shared.webpack.config');
const path = require('path');
const WasmPackPlugin = require('@wasm-tool/wasm-pack-plugin');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = withDefaults({
  context: path.join(__dirname),
  entry: {
    extension: './src/server.ts',
  },
  output: {
    filename: 'server.js',
    path: path.join(__dirname, 'out'),
  },
  plugins: [
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, '../../../cddl'),
      extraArgs: '--target nodejs -- --features lsp',
      forceMode: 'production',
      outDir: path.resolve(__dirname, 'pkg'),
      outName: 'cddl',
    }),

    new CopyPlugin({
      patterns: [{ from: 'pkg/cddl_bg.wasm', to: 'cddl_bg.wasm' }],
    }),
  ],
});
