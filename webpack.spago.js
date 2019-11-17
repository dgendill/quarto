const webpack = require('webpack'); //to access built-in plugins
const path = require('path');

module.exports = {
  entry: './output/Main/index.js',
  target: 'web',
  output: {
    filename: 'app.js',
    path: path.resolve(__dirname, 'dist'),
    sourceMapFilename: 'app.js.map',
    library: "PSGame",
    libraryTarget: "var"
  },
  externals: {
	// './node/window.js': 'window',
	// /node/extend.js': 'function(){}',
    // 'paper' : true
  },
  devtool : "#source-map"
  // resolve: {
  //   modules: ['bower_components'],
  //   descriptionFiles: ['package.json', 'bower.json'],
  // }
};
