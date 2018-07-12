require('./setup.js');

const { execFileSync } = require('child_process');

const defaultOptions = {
  ignoreError: false,
  production: false,
};

function buildOCaml(options = defaultOptions) {
  try {
    execFileSync('dune', ['runtest'], {
      stdio: 'inherit',
      env: { BISECT_ENABLE: 'YES' },
    });
    let files = execFileSync('find', ['.', '-name', '"bisect*.out"']);
    let args = ['-I', '_build/default', '-html', '_coverage'].concat(files);
    execFileSync('bisect-ppx-report', args, { stdio: 'inherit' });
  } catch (e) {
    if (!options.ignoreError) {
      throw e;
    }
  }
}

module.exports.buildOCaml = buildOCaml;

if (require.main === module) {
  buildOCaml();
}
