import gulp from 'gulp';
import glob from 'glob';
import shell from 'gulp-shell';
import seq from 'run-sequence';

let opts = {
  testDir: './test',
  testBundledDir: './test-bundled',
  testBundled: './test-bundled/test.bundled.js',
  rendererBundled: './src/bundle.js',
  rendererMain: './src/js/renderer/App.js',

  mainBundled: './src/main.js',
  mainMain: './src/js/main/Main.js'
};

function makeBundlerForTest(bundler) {
  var verbose = (bundler === 'watchify') ? '-v' : '';
  return function() {
    let sources = glob.sync('./test/**/*_test.js');
    sources = sources.join(' ');
    let file = opts.testBundled;
    let command = ` | exorcist ${file}.map > ${file}`;
    if (bundler === 'watchify') {
      command = ` -o 'exorcist ${file}.map > ${file}'`;
    }

    return shell.task([`${bundler} ${verbose} -t [ babelify --plugins babel-plugin-espower ] -t react-templatify ${sources} --node -d ${command}`])();
  };
}

function makeBundlerForRenderer(bundler) {
  var verbose = (bundler === 'watchify') ? '-v' : '';
  return function() {
    let file = opts.rendererBundled;
    let command = ` | exorcist ${file}.map > ${file}`;
    if (bundler === 'watchify') {
      command = ` -o 'exorcist ${file}.map > ${file}'`;
    }

    return shell.task([`${bundler} ${verbose} -t babelify -t react-templatify ${opts.rendererMain} -d ${command}`])();
  };
}

function makeBundlerForMain(bundler) {
  var verbose = (bundler === 'watchify') ? '-v' : '';
  return function() {
    let file = opts.rendererBundled;
    let command = ` | exorcist ${file}.map > ${file}`;
    if (bundler === 'watchify') {
      command = ` -o 'exorcist ${file}.map > ${file}'`;
    }

    return shell.task([`${bundler} ${verbose} -t babelify --im --node --no-detect-globals ${opts.mainMain} -d ${command}`])();
  };
}

gulp.task('mocha', shell.task([`mocha --colors --require ${opts.testDir}/setup.js ${opts.testBundled}`]));

gulp.task('mocha:watch', () => {
  gulp.watch([opts.testBundled], shell.task([`mocha --colors --require ${opts.testDir}/setup.js ${opts.testBundled}`], {
    ignoreErrors: true
  }));
});

// bundle test sources before test.
gulp.task('test:browserify', makeBundlerForTest('browserify'));
gulp.task('test:watchify', makeBundlerForTest('watchify'));

// execute test.
gulp.task('test', (done) => seq('test:browserify', 'mocha', done));
gulp.task('watch:test', (done) => seq(['test:watchify', 'mocha:watch'], done));

// watch and build main bundle
gulp.task('build:main:browserify', makeBundlerForMain('browserify'));
gulp.task('build:main:watchify', makeBundlerForMain('watchify'));

// watch and build renderer bundle
gulp.task('build:renderer:browserify', makeBundlerForRenderer('browserify'));
gulp.task('build:renderer:watchify', makeBundlerForRenderer('watchify'));

// watch and build css bundle
gulp.task('build:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));
gulp.task('watch:css', shell.task(['stylus -c -w ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));

gulp.task('lint', shell.task(['eslint -c ./.eslintrc src/js/**/*.js']));
gulp.task('watch:lint', shell.task(["watch 'eslint -c ./.eslintrc src/js/**/*.js' src/js"], {
  ignoreErrors: true
}));


// main build task.
gulp.task('build', (done) => seq(['build:main:browserify',
                                  'build:renderer:browserify',
                                  'build:css'
                                 ], done));

// main watch task
gulp.task('watch', (done) => seq(['build:main:watchify',
                                  'build:renderer:watchify',
                                  'watch:css',
                                  'watch:lint'
                                 ], done));
