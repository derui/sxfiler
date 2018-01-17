require('./setup.js');
const chokidar = require('chokidar');
const { execFileSync } = require('child_process');

// Run command to build scss.
// If this function gave an argument named watching, this function run a build command with watch option.
function buildCss(ignoreError = false) {
  let options = [
    '--include-path',
    'src/sass',
    '--importer',
    'node_modules/node-sass-glob-importer/dist/cli.js',
    '-o',
    './dist/web',
    './src/sass/app.scss',
  ];

  try {
    execFileSync('node-sass', options, { stdio: 'inherit' });
  } catch (e) {
    if (!ignoreError) {
      throw e;
    }
  }
}

// Run command to build scss with watching file changes.
// That why do not use --watch option of node-sass, because node-sass's --watch option is
// not working with importer.
// We want to compile only one file, so we decides to spawn process each file changes.
function buildCssWithWatch() {
  return chokidar
    .watch('src/sass/', { ignored: /(^|[\/\\])\../, ignoreInitial: true })
    .on('all', (event, path) => {
      buildCss(true);
    });
}

module.exports.buildCss = buildCss;
module.exports.buildCssWithWatch = buildCssWithWatch;

if (require.main === module) {
  (function() {
    let watched = false;

    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      buildCss(true);
      buildCssWithWatch();
    } else {
      buildCss();
    }
  })();
}
