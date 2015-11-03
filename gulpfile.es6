import gulp from 'gulp';
import glob from 'glob';
import shell from 'gulp-shell';
import seq from 'run-sequence';
import path from 'path';

let opts = {
  testDir: './test',
  rendererTestBundled: './test-bundled/test-renderer.bundled.js',
  rendererBundled: './src/bundle.js',
  rendererMain: './src/js/renderer/App.js',

  mainTestBundledDir: './test-bundled',
  mainTestBundled: './test-bundled/test-main.bundled.js',
  mainBundled: './src/main.js',
  mainMain: './src/js/main/Main.js'
};

function makeBundlerForRenderer(bundler, main, external) {
  var verbose = (bundler === 'watchify') ? '-v' : '';
  return function() {
    let command = `${external} `;
    if (bundler === 'watchify') {
      command = ` -o '${external}'`;
    }

    return shell.task([`${bundler} ${verbose} -t babelify -t react-templatify ${main} -d ${command}`])();
  };
}

function makeRendererBundler(bundler) {
  return makeBundlerForRenderer(
    bundler, opts.rendererMain, `| exorcist ${opts.rendererBundled}.map > ${opts.rendererBundled}`);
}

function makeRendererBundlerForTest(bundler, files) {
  return makeBundlerForRenderer(
    bundler, files.join(' '), `| tape-run | faucet`);
}

function makeBundlerForMain(bundler, main, target) {
  var verbose = (bundler === 'watchify') ? '-v' : '';
  return function() {
    let command = ` | exorcist ${target}.map > ${target}`;
    if (bundler === 'watchify') {
      command = ` -o 'exorcist ${target}.map > ${target}'`;
    }

    return shell.task([`${bundler} ${verbose} -t babelify --im --node --no-detect-globals ${main} -d ${command}`])();
  };
}

// bundle test sources before test.
gulp.task('test:renderer:browserify', () => {
  let sources = glob.sync('./test/renderer/**/*_test.js');
  return makeRendererBundlerForTest('browserify', sources)();
});
gulp.task('test:renderer:watchify', () => {
  let sources = glob.sync('./test/renderer/**/*_test.js');
  return makeRendererBundlerForTest('watchify', sources)();
});

gulp.task('test:js-main:browserify', () => {
  let sources = glob.sync('./test/main/**/*_test.js');
  return makeBundlerForMain('browserify', sources.join(' '), opts.mainTestBundled)();
});
gulp.task('test:js-main:watchify', () => {
  let sources = glob.sync('./test/main/**/*_test.js');
  return makeBundlerForMain('watchify', sources.join(' '), opts.mainTestBundled)();
});

gulp.task('test:js-main', shell.task([`tape ${opts.mainTestBundled} | faucet`]));
gulp.task('test:js-main:watch', shell.task([`tape ${opts.mainTestBundled} | faucet`]));
gulp.task('test:js-main:watch', shell.task([`watch 'tape ${opts.mainTestBundled}' ${opts.mainTestBundledDir}`], {
  ignoreErrors: true
}));

// execute test.
gulp.task('test', (done) => seq(
  ['test:js-main:browserify', 'test:renderer:browserify'], 'test:js-main', done
));
gulp.task('watch:test', (done) => seq(
  ['test:js-main:watchify', 'test:renderer:watchify', 'test:js-main:watch'], done));

// watch and build main bundle
gulp.task('build:js-main:browserify', makeBundlerForMain('browserify', opts.mainMain, opts.mainBundled));
gulp.task('build:js-main:watchify', makeBundlerForMain('watchify', opts.mainMain, opts.mainBundled));

// watch and build renderer bundle
gulp.task('build:renderer:browserify', makeRendererBundler('browserify'));
gulp.task('build:renderer:watchify', makeRendererBundler('watchify'));

// watch and build css bundle
gulp.task('build:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));
gulp.task('watch:css', shell.task(['stylus -c -w ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));
gulp.task('release:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css']));

gulp.task('lint', shell.task(['eslint -c ./.eslintrc src/js/**/*.js']));
gulp.task('watch:lint', shell.task(["watch 'eslint -c ./.eslintrc src/js/**/*.js' src/js"], {
  ignoreErrors: true
}));

// main build task.
gulp.task('build', (done) => seq(['build:js-main:browserify',
                                  'build:renderer:browserify',
                                  'build:css'
                                 ], done));

// main watch task
gulp.task('watch', (done) => seq(['build:js-main:watchify',
                                  'build:renderer:watchify',
                                  'watch:css',
                                  'watch:lint'
                                 ], done));

// release tasks
gulp.task('release:js-renderer', () => {
  return shell.task([`browserify -t babelify -t react-templatify ${opts.rendererMain} | uglifyjs -c warnings=false > ${opts.rendererBundled}`])();
});
gulp.task('release:js-main', () => {
  return shell.task([`browserify -t babelify --im --node --no-detect-globals ${opts.mainMain} | uglifyjs -c warnings=false > ${opts.mainBundled}`])();
});
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
  return shell.task([`cpx "./src/**/{${files}}" ./dist/src`])();
});

function packagePlatform(platform) {
  return shell.task([`electron-packager ./dist/src Starter --out=dist/bin --cache=dist/cache --platform=${platform} --arch=x64 --version=0.34.1 --overwrite --asar`]);
}
gulp.task('release:pack-osx', packagePlatform('darwin'));
gulp.task('release:pack-win', packagePlatform('win32'));
gulp.task('release:pack-linux', packagePlatform('linux'));

gulp.task('release', (done) => seq(
  'release:clean',
  ['release:css', 'release:js-main', 'release:js-renderer'],
  'release:copy',
  ['release:pack-osx', 'release:pack-win', 'release:pack-linux'],
  done));
