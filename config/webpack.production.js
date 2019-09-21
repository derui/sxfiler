'use strict';
process.env.NODE_ENV = 'production';

const configFactory = require('./webpack.config');

module.exports = configFactory('production');
