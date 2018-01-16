require('./setup.js');
const fs = require('fs');
const { execFileSync } = require('child_process');

execFileSync(
  'node-sass',
  [
    '--include-path',
    'src/sass',
    '--importer',
    'node_modules/node-sass-globbing/index.js',
    '-o',
    './dist/web',
    './src/sass/app.scss',
  ],
  { stdio: 'inherit' },
  (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  }
);
