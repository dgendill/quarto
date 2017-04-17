const webpack = require('webpack'); //to access built-in plugins
const path = require('path');

module.exports = {
  entry: './js/canvas/Game.js',
  target: 'web',
  output: {
    filename: 'webpack-bundle.js',
    path: path.resolve(__dirname, 'js'),
    sourceMapFilename: 'webpack-bundle.js.map',
    library: "Game",
    libraryTarget: "var"
  },
  externals: {
		'./node/window.js': 'window',
		'./node/extend.js': 'function(){}',
    'paper' : true
	},
  devtool : "#source-map"
  // resolve: {
  //   modules: ['bower_components'],
  //   descriptionFiles: ['package.json', 'bower.json'],
  // }
};
