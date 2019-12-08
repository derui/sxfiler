const util = require('util');

let { execFile } = require('child_process');
execFile = util.promisify(execFile);

const glob = require('glob');

glob('{src,test}/**/*.{ml,mli}', (er, files) => {
  async function formatting(f) {
    const ocamlFormatResult = await execFile('ocamlformat', ['-i', f]);
    if (ocamlFormatResult.stdout || ocamlFormatResult.stderr) {
      console.log(ocamlFormatResult.stdout, ocamlFormatResult.stderr);
    }
  }

  const promises = [
    execFile('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'scripts/*.js'], {
      stdio: 'inherit',
    }),
    execFile('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'public/*.js'], {
      stdio: 'inherit',
    }),
    execFile('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'config/*.js'], {
      stdio: 'inherit',
    }),

    execFile('prettier', ['--parser', 'typescript', '--write', 'src/ts/**/*.{ts,tsx}'], { stdio: 'inherit' }),
  ].concat(files.map(formatting));

  Promise.all(promises);
});
