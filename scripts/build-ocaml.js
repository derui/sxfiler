const { execFileSync } = require('child_process');

execFileSync(
  'jbuilder',
  ['build', '@js'],
  { stdio: 'inherit' },
  (error, stdout, stderr) => {
    if (error) {
      throw error;
    }
  }
);
