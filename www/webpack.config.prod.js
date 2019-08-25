const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');

module.exports = {
  entry: "./bootstrap.js",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "[name].bootstrap.js",
  },
  mode: "production",
  plugins: [
    new CopyWebpackPlugin(['index.html'])
  ],
  module: {
    rules: [{
      test: /\.css$/,
      use: ['style-loader', 'css-loader']
    }]
  }
};
