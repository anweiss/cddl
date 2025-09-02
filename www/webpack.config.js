const CopyWebpackPlugin = require('copy-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const webpack = require('webpack');
const path = require('path');

module.exports = {
  entry: './bootstrap.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js',
  },
  mode: 'development',
  resolve: {
    fallback: {
      "util": require.resolve("util/"),
      "path": require.resolve("path-browserify"),
      "fs": false,
      "process": require.resolve("process/browser"),
      "text-encoding": require.resolve("text-encoding")
    },
    alias: {
      'text-encoding': require.resolve('text-encoding')
    }
  },
  devServer: {
    static: [
      {
        directory: path.join(__dirname, 'dist'),
        publicPath: '/',
      }
    ],
    compress: true,
    port: 8080,
    open: true,
    hot: true,
    client: {
      logging: 'info',
    },
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
    new webpack.ProvidePlugin({
      process: 'process/browser',
      TextEncoder: ['text-encoding', 'TextEncoder'],
      TextDecoder: ['text-encoding', 'TextDecoder'],
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
