const { buildOCamlTest } = require('./build-ocaml-test.js');

const { execFileSync } = require('child_process');

function testWithMocha(bundled) {
  execFileSync('mocha', [bundled], { stdio: 'inherit' });
}

module.exports = testWithMocha;

if (require.main === module) {
  (function() {
    const bundledTests = ['_build/default/test/kbd/test_sxfiler_kbd.bc.js'];

    bundledTests.forEach(testWithMocha);
  })();
}
