const { buildOCamlTest } = require('./build-ocaml-test.js');

const { spawn } = require('child_process');

function testWithMocha(bundled) {
  spawn('mocha', [bundled], { stdio: 'inherit' });
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
    const bundledTests = ['_build/default/test/kbd/test_sxfiler_kbd.bc.js'];

    bundledTests.forEach(testWithMocha);
    testWithKarma();
  })();
}
