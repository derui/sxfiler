const path = require("path");
const glob = require("glob");

module.exports = {
  // どのファイルをビルドするのかを指定。複数可。
  entry: [
    './_build/default/test/virtualized_list/test_sxfiler_virtualized_list.bc.js'
  ],
  // 出力するファイル名と出力先パス
  output: {
    path: path.join(__dirname, '_build/default/test'),
    filename: 'test_bundle.js'
  },
  node: {
    fs: "empty",
    child_process: "empty"
  },
  devtool: 'eval',
  // requireで読み込むときのrootのpathを指定
  resolve: {
    extensions: ['.js'],
  },
}
