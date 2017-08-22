const gulp = require('gulp');
const glob = require('glob');
const shell = require('gulp-shell');
const seq = require('run-sequence');

let opts = {
  srcDir: './src/ts',
  testDir: './test',
  testBundledDir: './test-bundled',

  renderer: {
    testDir: './test/main',
    testBundled: './test-bundled/test-renderer.bundled.js',
    bundled: './src/bundle.js',
    main: './src/ts/renderer/app.js',
    options: '-t [ babelify --presets [ es2015 ] ] -t aliasify'
  },
  // this setting only used for tests for components
  components: {
    testDir: './test/components',
    testBundled: './test-bundled/test-components.bundled.js',
    options: '-t [ babelify --presets [ es2015 ] ] -t aliasify --im --node --no-detect-globals'
  },
  main: {
    testDir: './test/main',
    testBundled: './test-bundled/test-main.bundled.js',
    bundled: './src/main.js',
    main: './src/ts/main/main.js',
    options: '-t [ babelify --presets [ es2015 ] ] -t aliasify --im --node --no-detect-globals'
  }
};

// watch and build css bundle
gulp.task('build:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));
gulp.task('watch:css', (done) => {
  shell.task(['stylus -c -w ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus'])();
  done();
});
gulp.task('release:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css']));

// build OCaml via jbuilder
gulp.task('build:js', shell.task(['jbuilder build @js']));

// main build task.
gulp.task('build', (done) => seq(['build:jbuilder', 'build:css'], done));

// main watch task
gulp.task('watch:main', ['watch:css'],
    () => {
      gulp.watch([`${opts.srcDir}/**/*.ts`], ['tsc']);
      gulp.watch([`${opts.srcDir}/**/*.rt`], ['build:template']);
    });

gulp.task('watch', done => seq('watch:main', done));

// release tasks
gulp.task('release:clean', shell.task(['rimraf ./dist/src']));
gulp.task('release:copy', () => {
  let files = [
    'index.html',
    '*.eot',
    '*.svg',
    '*.ttf',
    '*.woff',
    'package.json'
  ].join(',');
  return shell.task([`cpx "./src/**/{${files}}" ./dist/src`]);
});

function packagePlatform(platform) {
  return shell.task([`electron-packager ./dist/src Starter --out=dist/bin --cache=dist/cache --platform=${platform} --arch=x64 --version=1.7.5 --overwrite --asar`]);
}
gulp.task('release:pack-osx', packagePlatform('darwin'));
gulp.task('release:pack-win', packagePlatform('win32'));
gulp.task('release:pack-linux', packagePlatform('linux'));

gulp.task('release', (done) => seq(
  'release:clean',
  'release:css',
  'release:copy',
  ['release:pack-osx', 'release:pack-win', 'release:pack-linux'],
  done));
