const util = require('util');

let { execFileSync, execFile } = require('child_process');
execFile = util.promisify(execFile);

const glob = require('glob');

glob('{src,test}/**/*.{ml,mli}', (er, files) => {
  async function formatting(f) {
    console.log(`Formatting OCaml source: ${f}`);
    const ocamlFormatResult = await execFile('ocamlformat', ['-i', f]);
    const ocpIndentResult = await execFile('ocp-indent', ['-i', f]);
    if (ocamlFormatResult.stdout || ocamlFormatResult.stderr) {
      console.log(ocamlFormatResult.stdout, ocamlFormatResult.stderr);
    }

    if (ocpIndentResult.stdout || ocpIndentResult.stderr) {
      console.log(ocpIndentResult.stdout, ocpIndentResult.stderr);
    }
  }
  Promise.all(files.map(formatting));
});

execFileSync('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'scripts/*.js'], {
  stdio: 'inherit',
});
execFileSync('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'public/*.js'], {
  stdio: 'inherit',
});
execFileSync('prettier', ['--single-quote', '--trailing-comma', 'es5', '--write', 'config/*.js'], {
  stdio: 'inherit',
});

execFileSync('prettier', ['--parser', 'typescript', '--write', 'src/ts/**/*.{ts,tsx}'], { stdio: 'inherit' });
