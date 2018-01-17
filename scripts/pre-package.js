const { execFileSync } = require('child_process');

let files = [
  'index.html',
  '*.eot',
  '*.svg',
  '*.ttf',
  '*.woff',
  'package.json',
].join(',');
execFileSync(
  'cpx',
  [`./src/{${files}}`, './dist'],
  { stdio: 'inherit' },
  (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  }
);
