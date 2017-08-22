require('./setup.js');
const { execFileSync } = require('child_process');

execFileSync('stylus', ['-c', './src/stylus/app.styl', '-o', './dist/web/bundle.css'], {stdio: 'inherit'}, (error, stdout, stderr) => {
  if (error) {
    throw error;
  }
});


