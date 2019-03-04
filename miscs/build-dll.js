require('./setup.js');
const { execFileSync } = require('child_process');

const defaultOptions = {
  production: false,
};

function bundleDll(options = defaultOptions) {
  let cmdOptions = ['--config', 'webpack.dll.config.js'];

  cmdOptions.push('--mode');
  if (options.production) {
    cmdOptions.push('production');
  } else {
    cmdOptions.push('development');
  }
  execFileSync('webpack', cmdOptions, { stdio: 'inherit' });

  execFileSync('cpx', [`vendor/dll.react.js`, './dist/web'], {
    stdio: 'inherit',
  });
}

module.exports.bundleDll = bundleDll;

if (require.main === module) {
  (function() {
    let options = Object.assign({}, defaultOptions);
    const argv = process.argv.slice(2);
    argv.forEach(arg => {
      switch (arg) {
        case 'prod':
          options.production = true;
          break;
        default:
          break;
      }
    });

    bundleDll(options);
  })();
}
