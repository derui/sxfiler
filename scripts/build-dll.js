require('./setup.js');
const { execFileSync } = require('child_process');

function bundleDll() {
  let options = ['--config', 'webpack.dll.config.js'];

  execFileSync('webpack', options, { stdio: 'inherit' });

    execFileSync(
      'cpx',
      [`vendor/dll.react.js`, './dist/web'],
      { stdio: 'inherit' }
    );
}

module.exports.bundleDll = bundleDll;

if (require.main === module) {
  (function() {
    bundleDll();
  })();
}
