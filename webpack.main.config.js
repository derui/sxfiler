const path = require('path');
module.exports = {
  target: 'web',
  entry: './_build/default/src/ocaml/renderer/sxfiler_renderer.bc.js',
  output: {
    path: path.resolve(__dirname, 'dist', 'web'),
    filename: 'bundle.js',
    libraryTarget: "commonjs2",
  },
  externals: [
    "electron",
    'fs',
  ]
};
