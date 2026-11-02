//@ts-check

'use strict';

const withDefaults = require('../shared.webpack.config');
const path = require('path');
const { applyPatch } = require('./scripts/patch-languageclient.cjs');

applyPatch({ check: true });

module.exports = withDefaults({
  context: path.join(__dirname),
  entry: {
    extension: './src/extension.ts',
  },
  output: {
    filename: 'extension.js',
    path: path.join(__dirname, 'out'),
  },
});
