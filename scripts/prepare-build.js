'use strict';

process.env.NODE_ENV = 'production';

process.on('unhandledRejection', (err) => {
  throw err;
});

// Ensure environment variables are read.
require('../config/env');

const path = require('path');
const fs = require('fs-extra');
const paths = require('../config/paths');

function copyPublicFolder() {
  fs.copySync(paths.appPublic, paths.appBuild, {
    dereference: true,
    filter: (file) => file !== paths.appHtml,
  });
}

fs.emptyDirSync(paths.appBuild);
// Merge with the public folder
copyPublicFolder();
