require('./build-css.js');
require('./build-ocaml.js');
require('./bundle-main.js');
require('./pre-package.js');

const { execFileSync } = require('child_process');

execFileSync(
  'electron',
  ['./dist'],
  { stdio: 'inherit' },
  (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  }
);
