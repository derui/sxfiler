'use strict';
process.env.NODE_ENV = 'development';

const configFactory = require('./webpack.config');
const createDevServerConfig = require('./webpackDevServer.config');

module.exports = Object.assign({}, configFactory("development"), {
  devServer: createDevServerConfig()
});
