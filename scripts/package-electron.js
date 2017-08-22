require('./build-css.js');
require('./build-ocaml.js');
require('./bundle-main.js');
require('./pre-package.js');

const { execFileSync } = require('child_process');

const platforms = [
  'darwin',
  'win32',
  'linux'];

platforms.forEach(platform => {
  const options = ['./dist', 'sxfiler', `--platform=${platform}`, '--arch=x64',
                   '--electron-version=1.7.5', '--overwrite', '--asar']
  execFileSync('electron-packager', options, {stdio: 'inherit'}, (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  });
})
