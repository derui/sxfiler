const {copyFiles} = require('./pre-package.js');

const { buildCss } = require('./build-css.js');
const { buildOCaml } = require('./build-ocaml.js');
const { buildCommonLisp } = require('./build-cl.js');

function getPlatforms() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    return ['darwin', 'win32', 'linux'];
  } else {
    return args;
  }
}

const { execFileSync } = require('child_process');

copyFiles(true);
buildCss();
buildOCaml();
buildCommonLisp();

const platforms = getPlatforms();

platforms.forEach(platform => {
  const options = [
    './dist',
    'sxfiler',
    `--platform=${platform}`,
    '--arch=x64',
    '--electron-version=1.7.5',
    '--overwrite',
    '--asar',
  ];
  execFileSync('electron-packager', options, { stdio: 'inherit' });
});
