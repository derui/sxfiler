require('./setup.js');
const fs = require('fs');
const { spawn } = require('child_process');

// Run command to build scss.
// If this function gave an argument named watching, this function run a build command with watch option.
function buildCss(watching) {
  let options = [
    '--include-path',
    'src/sass',
    '--recursive',
    '--importer',
    'node_modules/node-sass-globbing/index.js',
    '-o',
    './dist/web',
    './src/sass/app.scss',
  ];

  if (watching) {
    options.unshift('--watch');
  }

  return spawn(
    'node-sass',
    options,
    { stdio: 'inherit' },
    (error, stdout, stderr) => {
      if (error) {
        throw error;
      }
    }
  );
}

module.exports = buildCss;

if (require.main === module) {
  (function() {
    let watched = false;

    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      watched = true;
    }

    let child = buildCss(watched);
    child.on('exit', (exit, signal) => {
      if (exit !== null) {
        process.exit(exit);
      } else {
        console.warn(signal);
        process.exit(1);
      }
    });
  })();
}
