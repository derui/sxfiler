const path = require('path');
const webpack = require('webpack');


module.exports = {
  entry: {
    react: ["react", "react-dom"],
  },
  output: {
    path: path.join(__dirname, "vendor"),
    filename: "dll.[name].js",
    library: "[name]_[hash]"
  },
  plugins: [
    new webpack.DllPlugin({
      path: path.join(__dirname, "vendor", "[name]-manifest.json"),
      name: "[name]_[hash]"
    })
  ]
}
