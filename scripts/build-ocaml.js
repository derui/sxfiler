require('./setup.js');
const chokidar = require('chokidar');

const { execFileSync } = require('child_process');

function bundleRenderer() {
  let options = ['--cache', '--config', 'webpack.main.config.js'];

  execFileSync('webpack', options, { stdio: 'inherit' });
}

function buildOCaml(ignoreError = false) {
  try {
    execFileSync('jbuilder', ['build', '@js'], { stdio: 'inherit' });

    bundleRenderer();

    execFileSync(
      'cpx',
      [`./_build/default/src/ocaml/main/sxfiler_main.bc.js`, './dist'],
      { stdio: 'inherit' }
    );
  } catch (e) {
    if (!ignoreError) {
      throw e;
    }
  }
}

function buildOCamlWithWatch() {
  // One-liner for current directory, ignores .dotfiles
  return chokidar
    .watch('src/ocaml/', { ignored: /(^|[\/\\])\../, ignoreInitial: true })
    .on('all', (event, path) => {
      buildOCaml(true);
    });
}

module.exports.buildOCaml = buildOCaml;
module.exports.buildOCamlWithWatch = buildOCamlWithWatch;

if (require.main === module) {
  (function() {
    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      buildOCaml(true);
      buildOCamlWithWatch();
    } else {
      buildOCaml();
    }
  })();
}
