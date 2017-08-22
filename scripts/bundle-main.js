require('./setup.js');
const { execFileSync } = require('child_process');

execFileSync('webpack', ['--config', 'webpack.main.config.js'], {stdio: 'inherit'}, (error, stdout, stderr) => {
  if (error) {
    throw error;
  }
});
