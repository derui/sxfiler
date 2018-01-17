require('./pre-package.js');

const { buildCssWithWatch } = require('./build-css.js');
const { buildOCamlWithWatch } = require('./build-ocaml.js');
const bundleRenderer = require('./bundle-main.js');

const { spawn } = require('child_process');

function runApp() {
  return spawn(
    'electron',
    ['./dist'],
    { stdio: 'inherit' },
    (error, stdout, stderr) => {
      if (error) {
        throw error;
      }
    }
  );
}

module.exports = runApp;

if (require.main === module) {
  (function() {
    const cssFileWatcher = buildCssWithWatch();
    const ocamlFileWatcher = buildOCamlWithWatch();
    const cpBundler = bundleRenderer(true);

    const cpRunApp = runApp();
    cpRunApp.on('exit', (exit, signal) => {
      cssFileWatcher.close();
      ocamlFileWatcher.close();
      cpBundler.kill();

      if (exit !== null) {
        exit(exit);
      } else {
        console.warn(signal);
        exit(1);
      }
    });
  })();
}
