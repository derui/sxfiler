require('./build-css.js');
require('./build-ocaml.js');
require('./bundle-main.js');
require('./pre-package.js');

function getPlatforms() {
  const args = process.argv.slice(2)

  if (args.length === 0) {
    return [
      'darwin',
      'win32',
      'linux']
  } else {
    return args
  }
}

const { execFileSync } = require('child_process');

const platforms = getPlatforms()

platforms.forEach(platform => {
  const options = ['./dist', 'sxfiler', `--platform=${platform}`, '--arch=x64',
                   '--electron-version=1.7.5', '--overwrite', '--asar']
  execFileSync('electron-packager', options, {stdio: 'inherit'}, (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  });
})
