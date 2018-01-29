const { execFileSync } = require('child_process');

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
