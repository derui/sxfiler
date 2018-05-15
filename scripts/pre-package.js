const { execFileSync } = require('child_process');
const fs = require('fs');


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

  let indexName = 'index_dev.js';
  if (production) {
    indexName = 'index_prod.js';
  }

  execFileSync('cpx', [`./src/${indexName}`, './dist'], {stdio: 'inherit'});
  fs.renameSync(`./dist/${indexName}`, "./dist/index.js");
}

module.exports.copyFiles = copyFiles;
