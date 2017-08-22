const path = require('path');
module.exports = {
  entry: './_build/default/src/ocaml/renderer/sxfiler_renderer.bc.js',
  output: {
    path: path.resolve(__dirname, 'dist', 'web'),
    filename: 'bundle.js'
  },
  node: {
    fs: "empty"
  },
};
