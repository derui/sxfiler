const webpack = require('webpack');
const path = require('path');
const output = {
  path: path.resolve(__dirname, 'dist', 'web'),
  filename: 'bundle.js',
  libraryTarget: "commonjs2"
};

const FunctionModulePlugin = require('webpack/lib/FunctionModulePlugin');
const NodeTargetPlugin = require('webpack/lib/node/NodeTargetPlugin');

module.exports = {
  bail: false,
  entry: './_build/default/src/ocaml/renderer/sxfiler_renderer.bc.js',
  output: output,

  target: function(compiler) {
    compiler.apply(
      new webpack.JsonpTemplatePlugin(output),
      new FunctionModulePlugin(output)
    )
  },

  plugins: [
    new NodeTargetPlugin(),
    new webpack.ExternalsPlugin("commonjs", [
      "desktop-capturer",
      "electron",
      "ipc",
      "ipc-renderer",
      "remote",
      "web-frame",
      "clipboard",
      "crash-reporter",
      "native-image",
      "screen",
      "shell"
    ]),
    new webpack.DllReferencePlugin({
      context: __dirname,
      manifest: require('./vendor/react-manifest.json')
    })
  ]
};
