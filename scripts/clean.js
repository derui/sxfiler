const fs = require('fs');
const execFileSync = require('child_process').execFileSync;

execFileSync('rimraf', ['./dist'], { stdio: 'inherit' });