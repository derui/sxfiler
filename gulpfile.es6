import gulp from 'gulp';
import glob from 'glob';
import shell from 'gulp-shell';
import seq from 'run-sequence';
import R from 'ramda';

let opts = {
  srcDir: './src/ts',
  testDir: './test',
  testBundledDir: './test-bundled',

  template: {
    rtOptions: '--react-import-path react -t 0.14.0 -m es6',
    targets: () => glob.sync('./src/ts/**/*.rt')
  },

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

function makeBundler(bundler, opts, external) {
  let verbose = (bundler === 'watchify') ? '-v' : '';
  let {main, options} = opts;
  let command = `${external} `;
  if (bundler === 'watchify') {
    command = ` -o '${external}'`;
  }

  return shell.task([`${bundler} ${verbose} ${options} ${main} -d ${command}`]);
}

function makeRendererBundler(bundler, opts) {
  return makeBundler(
    bundler, opts, `| exorcist ${opts.bundled}.map > ${opts.bundled}`);
}

// make bundle of renderer to run test on node. This function is only used to components built
// with React.js.
function makeRendererBundlerForComponentTest(bundler, opts) {
  return makeBundler(bundler, opts, `| node | faucet`);
}

// make bundle of renderer to run test on electron
function makeRendererBundlerForBrowserTest(bundler, opts) {
  return makeBundler(bundler, opts, `| tape-run | faucet`);
}

gulp.task('tsc', shell.task(`tsc -p ./src/ts`, {
  ignoreErrors: true
}));

// bundle test sources before test.
gulp.task('test:js-component:browserify', done => {
  let sources = glob.sync('./test/components/**/*_test.js');
  let mainOpts = R.merge(opts.components, {
    main: sources.join(' ')
  });
  makeRendererBundlerForComponentTest('browserify', mainOpts)(done);
});

gulp.task('test:js-component:watchify', done => {
  let sources = glob.sync('./test/components/**/*_test.js');
  let mainOpts = R.merge(opts.components, {
    main: sources.join(' ')
  });
  makeRendererBundlerForComponentTest('watchify', mainOpts)();
  done();
});

gulp.task('test:js-renderer:browserify', done => {
  let sources = glob.sync('./test/renderer/**/*_test.js');
  let mainOpts = R.merge(opts.renderer, {
    main: sources.join(' ')
  });
  makeRendererBundlerForBrowserTest('browserify', mainOpts)(done);
});

gulp.task('test:js-renderer:watchify', done => {
  let sources = glob.sync('./test/renderer/**/*_test.js');
  let mainOpts = R.merge(opts.renderer, {
    main: sources.join(' ')
  });
  makeRendererBundlerForBrowserTest('watchify', mainOpts)();
  done();
});

gulp.task('test:js-main:browserify', done => {
  let sources = glob.sync('./test/main/**/*_test.js');
  sources = sources.concat(glob.sync('./test/common/**/*_test.js'));
  let mainOpts = R.merge(opts.main, {
    main: sources.join(' '),
    bundled: opts.main.testBundled
  });
  makeBundler('browserify', mainOpts, `| node | faucet`)(done);
});

gulp.task('test:js-main:watchify', done => {
  let sources = glob.sync('./test/main/**/*_test.js');
  sources = sources.concat(glob.sync('./test/common/**/*_test.js'));
  let mainOpts = R.merge(opts.main, {
    main: sources.join(' '),
    bundled: opts.main.testBundled
  });
  makeBundler('watchify', mainOpts, `| node | faucet`)();
  done();
});

// execute test.
gulp.task('test', (done) => seq(
  ['tsc', 'build:template'],
  ['test:js-main:browserify', 'test:js-component:browserify', 'test:js-renderer:browserify'],
  done
));

gulp.task(
  'watch:test:actual',
  ['test:js-main:watchify',
   'test:js-component:watchify',
   'test:js-renderer:watchify'],
  () => {
    gulp.watch([`${opts.srcDir}/**/*.ts`], ['tsc']);
    gulp.watch([`${opts.srcDir}/**/*.rt`], ['build:template']);
  });

gulp.task('watch:test', done => seq('tsc', 'watch:test:actual', done));

// watch and build main bundle
gulp.task('build:js-main:browserify', makeBundler('browserify', opts.main, ` | exorcist ${opts.main.bundled}.map > ${opts.main.bundled}`));
gulp.task('build:js-main:watchify', (done) => {
  makeBundler('watchify', opts.main, ` | exorcist ${opts.main.bundled}.map > ${opts.main.bundled}`)();
  done();
});

// watch and build renderer bundle
gulp.task('build:js-renderer:browserify', makeRendererBundler('browserify', opts.renderer));
gulp.task('build:js-renderer:watchify', (done) => {
  makeRendererBundler('watchify', opts.renderer)();
  done();
});

gulp.task('build:template', 
  shell.task([`rt ${opts.template.targets().join(' ')} ${opts.template.rtOptions}`])
);

// watch and build css bundle
gulp.task('build:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus']));
gulp.task('watch:css', (done) => {
  shell.task(['stylus -c -w ./src/stylus/app.styl -o ./src/bundle.css -m --sourcemap-root ./stylus'])();
  done();
});
gulp.task('release:css', shell.task(['stylus -c ./src/stylus/app.styl -o ./src/bundle.css']));

gulp.task('lint', shell.task(['tslint -c ./tslint.json src/ts/**/*.ts']));

// main build task.
gulp.task('build', (done) => seq(
  ['tsc', 'build:template'],
  ['build:js-main:browserify',
   'build:js-renderer:browserify',
   'build:css'
  ], done));

// main watch task
gulp.task(
  'watch:main',
  ['build:js-renderer:watchify', 'build:js-main:watchify', 'watch:css'],
  () => {
    gulp.watch([`${opts.srcDir}/**/*.ts`], ['tsc']);
    gulp.watch([`${opts.srcDir}/**/*.rt`], ['build:template']);
  });

gulp.task('watch', done => seq('tsc', 'watch:main', done));

// release tasks
gulp.task('release:js-renderer', shell.task([`browserify ${opts.renderer.options} ${opts.renderer.main} | uglifyjs -c warnings=false > ${opts.renderer.bundled}`]));
gulp.task('release:js-main', shell.task([`browserify ${opts.main.options} ${opts.main.main} | uglifyjs -c warnings=false > ${opts.main.bundled}`])());
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
