const webpack = require('webpack')
const ExtractTextPlugin = require("extract-text-webpack-plugin")
const HtmlWebpackPlugin = require('html-webpack-plugin')
const path = require('path')

module.exports = {
  entry: './lib/js/src/Main.js',
  output: {
    path: __dirname + "/dist",
    filename: "[name].js"
  },
  module: {
    loaders: [
    {
      test: /\.css$/,
      loader: ExtractTextPlugin.extract("css-loader")
    },
    {
      test: /\.less$/,
      loader: ExtractTextPlugin.extract("css-loader!less-loader")
    },
    {
      test: /\.scss$/,
      loader: ExtractTextPlugin.extract("css-loader!sass-loader")
    },
    { 
      test: /\.(jpg|png)$/, 
      loader: [ "url-loader?limit=10000" ] 
    }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
    new ExtractTextPlugin('[name].css'),
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, 'src', 'index.html')
    })
  ],
  devServer: {
    port: 8080,
    contentBase: path.join(__dirname, "dist")
  }
}
