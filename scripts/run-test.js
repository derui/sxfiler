const { buildOCamlTest } = require('./build-ocaml-test.js');

const { spawn } = require('child_process');
const glob = require('glob');

function testWithMocha(bundled) {
  spawn('mocha', bundled, { stdio: 'inherit' });
}

function testWithKarma() {
  spawn(
    'karma',
    [
      'start',
      '--single-run',
      '--browsers',
      'ChromeHeadlessNoSandbox',
      'karma.conf.js',
    ],
    {
      stdio: 'inherit',
    }
  );
}

module.exports.testWithMocha = testWithMocha;
module.exports.testWithKarma = testWithKarma;

if (require.main === module) {
  (function() {
    let bundledTests = glob.sync('_build/default/test/**/test_*.bc.js', {
      ignore: '_build/default/test/*virtualized*/test_*.bc.js',
    });

    testWithMocha(bundledTests);
    testWithKarma();
  })();
}
