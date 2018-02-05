require('./setup.js');
const chokidar = require('chokidar');

const { execFileSync } = require('child_process');

const defaultOptions = {
  ignoreError: false,
  production: false,
};

function bundleRenderer(options) {
  let cmdOptions = ['--cache', '--config', 'webpack.main.config.js'];

  if (options.production) {
    cmdOptions.push("-p");
  }

  execFileSync('webpack', cmdOptions, { stdio: 'inherit' });
}

function buildOCaml(options=defaultOptions) {
  try {
    execFileSync('jbuilder', ['build', '@js'], { stdio: 'inherit' });

    bundleRenderer(options);

    execFileSync(
      'cpx',
      [`./_build/default/src/ocaml/main/sxfiler_main.bc.js`, './dist'],
      { stdio: 'inherit' }
    );
  } catch (e) {
    if (!options.ignoreError) {
      throw e;
    }
  }
}

function buildOCamlWithWatch() {
  // One-liner for current directory, ignores .dotfiles
  return chokidar
    .watch('src/ocaml/', { ignored: /(^|[\/\\])\../, ignoreInitial: true })
    .on('all', (event, path) => {
      buildOCaml(Object.assign({}, defaultOptions, {ignoreError: true}));
    });
}

module.exports.buildOCaml = buildOCaml;
module.exports.buildOCamlWithWatch = buildOCamlWithWatch;

if (require.main === module) {
  (function() {
    let options = Object.assign({}, defaultOptions);
    const argv = process.argv.slice(2);
    argv.forEach(arg => {
      switch (arg) {
      case "watch": options.ignoreError = true; break;
      case "prod": options.producion = true; break;
      default: break;
      }
    });

    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      buildOCaml(Object.assign({}, options, {ignoreError: true}));
      buildOCamlWithWatch(options);
    } else {
      buildOCaml(options);
    }
  })();
}
