const HtmlWebpackPlugin = require('html-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: './bootstrap.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js',
  },
  mode: 'development',
  devtool: 'source-map',
  resolve: {
    alias: {
      // Only ship the editor and JSON support, not every built-in language.
      'monaco-editor$': require.resolve('monaco-editor/editor/editor.api.js'),
    },
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'index.html',
      inject: 'body',
      scriptLoading: 'module',
    }),
    new MonacoWebpackPlugin({
      // Monaco's package exports hide esm/metadata.js from package-name resolution.
      monacoEditorPath: path.join(__dirname, 'node_modules/monaco-editor'),
      languages: ['json'],
    }),
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
