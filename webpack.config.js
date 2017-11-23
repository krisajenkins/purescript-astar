'use strict';

const ExtractTextPlugin = require("extract-text-webpack-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');

const isWebpackDevServer = process.argv.filter(a => path.basename(a) === 'webpack-dev-server').length;
const isWatch = process.argv.filter(a => a === '--watch').length;

const plugins =
      isWebpackDevServer || !isWatch ? [] : [
        function(){
          this.plugin('done', function(stats){
            process.stderr.write(stats.toString('errors-only'));
          });
        }
      ]
;

module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: path.join(__dirname, "dist"),
    compress: true,
    port: 8000
  },

  entry: './entry.js',

  output: {
    path: path.join(__dirname, 'dist'),
    pathinfo: true,
    filename: 'app.[hash].js'
  },

  module: {
    rules: [
      { test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: "url-loader?limit=10000&mimetype=application/font-woff" },
      { test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: "file-loader" },
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs'
              ],
              bundle: false,
              watch: isWebpackDevServer || isWatch,
              pscIdeServer: true
            }
          }
        ]
      },
      {
        test: /\.less$/,
        use: [{
          loader: "style-loader" // creates style nodes from JS strings
        }, {
          loader: "css-loader" // translates CSS into CommonJS
        }, {
          loader: "less-loader" // compiles Less to CSS
        }]
      }
    ]
  },

  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    extensions: [ '.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),
    new HtmlWebpackPlugin({
      template: 'static/index.html'
    })
  ].concat(plugins)
};
