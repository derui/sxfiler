require('./setup.js');
const chokidar = require('chokidar');

const { execFileSync } = require('child_process');

const defaultOptions = {
  ignoreError: false,
  production: false,
};

function buildCommonLisp(options = defaultOptions) {
  try {
    execFileSync(
      'qlot',
      [
        'exec',
        'ros',
        '+Q',
        'dump',
        'executable',
        './src/common-lisp/roswell/sxfiler.ros',
        './dist/sxfiler-server',
      ],
      { stdio: 'inherit' }
    );
  } catch (e) {
    if (!options.ignoreError) {
      throw e;
    }
  }
}

module.exports.buildCommonLisp = buildCommonLisp;

if (require.main === module) {
  let options = Object.assign({}, defaultOptions);
  buildCommonLisp(options);
}
