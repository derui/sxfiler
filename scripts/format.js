const shell = require('shelljs');

let files = shell.find(`${__dirname}/../src/`, `${__dirname}/../test/`).filter(f => f.match(/\.ml$/));
files.forEach(f => shell.exec(`ocp-indent -i ${f}`));
