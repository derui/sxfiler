const packager = require('electron-packager');
const util = require('util');
const rimraf = require('rimraf');

let { execFileSync, execFile } = require('child_process');
execFile = util.promisify(execFile);

const glob = require('glob');
const paths = require('../config/paths');
const fs = require('fs-extra');
const path = require('path');

function getPlatforms() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    return ['darwin', 'win32', 'linux'];
  } else {
    return args;
  }
}

async function bundleElectronApp(options) {
  const appPaths = await packager(options);
  console.log(`Electron app bundles created:\n${appPaths.join('\n')}`);
}

const platforms = getPlatforms();

platforms.map(platform => {
  rimraf.sync(path.join(paths.appBuild, 'sxfiler_server*'));
  switch (platform) {
    case 'linux':
      fs.copySync(
        path.resolve('_build', 'install', 'default', 'bin', 'sxfiler_server'),
        path.join(paths.appBuild, 'sxfiler_server'),
        {
          dereference: true,
          filter: file => file !== paths.appHtml,
        }
      );
      break;
    case 'win32':
      fs.copySync(
        path.resolve('_build', 'install', 'default.windows', 'bin', 'sxfiler_server'),
        path.join(paths.appBuild, 'sxfiler_server'),
        {
          dereference: true,
          filter: file => file !== paths.appHtml,
        }
      );
      break;
  }

  const options = {
    dir: './build',
    out: './dist',
    executableName: 'sxfiler',
    platform,
    arch: 'x64',
    overwrite: true,
    asar: {
      unpack: 'sxfiler_server',
    },
  };
  bundleElectronApp(options);
});
