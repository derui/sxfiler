const paths = require('../config/paths');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');

module.exports = ({config, mode}) => {
  craConfig = require("../config/webpack.config.js")(config.mode);
  config.module.rules.push({
    test: /\.(ts|tsx)$/,
    include: paths.appSrc,
    use: [
      {
        loader: require.resolve('ts-loader'),
        options: {
          // disable type checker - we will use it in fork plugin
          configFile: paths.appTsConfig,
          transpileOnly: true
        }
      },
    ],
  });

  config.resolve.extensions.push('.ts', '.tsx');
  config.resolve.plugins = [new TsconfigPathsPlugin()];

  return config;
};
