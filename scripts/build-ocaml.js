require('./setup.js');
const chokidar = require('chokidar');

const { execFileSync } = require('child_process');

function buildOCaml() {
  execFileSync(
    'jbuilder',
    ['build', '@js'],
    { stdio: 'inherit' },
    (error, stdout, stderr) => {
      if (error) {
        throw error;
      }
    }
  );

  execFileSync(
    'cpx',
    [`./_build/default/src/ocaml/main/sxfiler_main.bc.js`, './dist'],
    { stdio: 'inherit' },
    (error, stdout, stderr) => {
      if (error) {
        throw error;
      }
    }
  );
}

function buildOCamlWithWatch() {
  // One-liner for current directory, ignores .dotfiles
  chokidar
    .watch('src/ocaml/', { ignored: /(^|[\/\\])\../ })
    .on('all', (event, path) => {
      buildOCaml();
    });
}

module.exports.buildOCaml = buildOCaml;
module.exports.buildOCamlWithWatch = buildOCamlWithWatch;

if (require.main === module) {
  (function() {
    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      buildOCamlWithWatch();
    } else {
      buildOCaml();
    }
  })();
}
