require('./setup.js');
const { spawn } = require('child_process');

function bundleRenderer(watching) {
  let options = ['--config', 'webpack.main.config.js'];

  if (watching) {
    options.unshift('--watch');
  }

  return spawn(
    'webpack',
    options,
    { stdio: 'inherit' },
    (error, stdout, stderr) => {
      if (error) {
        throw error;
      }
    }
  );
}

module.exports = bundleRenderer;

if (require.main === module) {
  (function() {
    let watching = false;

    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      watching = true;
    }

    let child = bundleRenderer(watching);

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
