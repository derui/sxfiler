const fs = require('fs');
const util = require('util');
const execFileSync = require('child_process').execFileSync;

execFileSync('rimraf', ['./dist'], { stdio: 'inherit' });
fs.mkdirSync('./dist');
fs.mkdirSync('./dist/web');
