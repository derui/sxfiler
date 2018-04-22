const { execFileSync } = require('child_process');


function copyFiles(production) {
  let files = [
    'index.html',
    '*.eot',
    '*.svg',
    '*.ttf',
    '*.woff',
    'package.json',
    'default.json',
  ].join(',');

  execFileSync('cpx', [`./src/{${files}}`, './dist'], { stdio: 'inherit' });

  if (production) {
    execFileSync('cpx', ['./src/index_prod.js', './dist/index.js'], {stdio: 'inherit'});
  } else {
    execFileSync('cpx', ['./src/index_dev.js', './dist/index.js'], {stdio: 'inherit'});
  }
}

module.exports.copyFiles = copyFiles;
