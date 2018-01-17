const { execFileSync } = require('child_process');

const glob = require('glob');

glob('src/**/*.{ml,mli}', (er, files) => {
  files.forEach(f => {
    console.log(`Formatting OCaml source: ${f}`);
    execFileSync('ocp-indent', ['-i', f], { stdio: 'inherit' });
  });
});

execFileSync(
  'prettier',
  ['--single-quote', '--trailing-comma', 'es5', '--write', 'scripts/*.js'],
  { stdio: 'inherit' }
);

execFileSync('prettier', ['--write', 'src/sass/**/*.scss'], {
  stdio: 'inherit',
});
