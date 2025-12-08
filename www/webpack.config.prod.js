const CopyWebpackPlugin = require('copy-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: './bootstrap.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js',
  },
  mode: 'production',
  resolve: {
    fallback: {
      "util": require.resolve("util/"),
      "path": require.resolve("path-browserify"),
      "fs": false
    }
  },
  plugins: [
    new CopyWebpackPlugin({ 
      patterns: [
        'index.html',
        {
          from: '../pkg/cddl_bg.wasm',
          to: 'cddl_bg.wasm'
        }
      ] 
    }),
    new MonacoWebpackPlugin(),
  ],
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  experiments: {
    asyncWebAssembly: true,
  },
};
