const { execFileSync } = require('child_process');

function bundleTest() {
  let options = [
    '--mode',
    'development',
    '--cache',
    '--config',
    'webpack.test.config.js',
  ];

  execFileSync('webpack', options, { stdio: 'inherit' });
}

function buildOCamlTest(ignoreError = false) {
  try {
    execFileSync('jbuilder', ['runtest'], { stdio: 'inherit' });

    bundleTest();
  } catch (e) {
    if (!ignoreError) {
      throw e;
    }
  }
}

function buildOCamlTestWithWatch() {
  // One-liner for current directory, ignores .dotfiles
  return chokidar
    .watch('test/', { ignored: /(^|[\/\\])\../, ignoreInitial: true })
    .on('all', (event, path) => {
      buildOCamlTest(true);
    });
}

module.exports.buildOCamlTest = buildOCamlTest;
module.exports.buildOCamlTesteWithWatch = buildOCamlTestWithWatch;

if (require.main === module) {
  (function() {
    if (process.argv.length > 2 && process.argv[2] === 'watch') {
      buildOCamlTest(true);
      buildOCamlTestWithWatch();
    } else {
      buildOCamlTest();
    }
  })();
}
