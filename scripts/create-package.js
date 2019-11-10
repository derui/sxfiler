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

// Get suffix for a archive from the platform
function archiveSuffixOfPlatform(platform) {
  switch (platform) {
    case 'linux':
      return 'tar.gz';
    case 'win32':
      return 'zip';
    default:
      throw Error(`Unknown platform: ${platform}`);
  }
}

// Create archiver for the platform
function createArchiverForPlatform(platform) {
  const archiver = require('archiver');
  switch (platform) {
    case 'linux':
      return archiver('tar', { gzip: true });
    case 'win32':
      return archiver('zip');
    default:
      throw Error(`Unknown platform: ${platform}`);
  }
}

async function bundleElectronApp(options, platform) {
  const appPaths = await packager(options);
  console.log(`Electron app bundles created:\n${appPaths.join('\n')}`);
  console.log(`Archiving for ${appPaths[0]}...`);

  const output = fs.createWriteStream(`${appPaths[0]}.${archiveSuffixOfPlatform(platform)}`);
  const archive = createArchiverForPlatform(platform);
  archive.pipe(output);

  archive.directory(appPaths[0], path.parse(appPaths[0]).base);
  archive.finalize();

  output.on('close', () => {
    console.log('Archiving finished');
  });
}

const platforms = getPlatforms();

platforms.map(platform => {
  rimraf.sync(path.join(paths.appBuild, 'sxfiler_server*'));
  switch (platform) {
    case 'linux':
      fs.copySync(
        path.resolve('_build', 'install', 'default', 'bin', 'sxfiler_server'),
        path.join(paths.appBuild, 'sxfiler_server.exe'),
        {
          dereference: true,
          filter: file => file !== paths.appHtml,
        }
      );
      break;
    case 'win32':
      fs.copySync(
        path.resolve('_build', 'install', 'default.windows', 'bin', 'sxfiler_server'),
        path.join(paths.appBuild, 'sxfiler_server.exe'),
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
      unpack: 'sxfiler_server.exe',
    },
  };
  bundleElectronApp(options, platform);
});
