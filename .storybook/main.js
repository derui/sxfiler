// automatically import all files ending in *.stories.tsx
const path = require("path")
const paths = require('../config/paths');
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');

module.exports = {
  stories: ['../src/stories/**/*.stories.tsx'],
  webpackFinal: (config) => {
    config.module.rules.push({
      test: /\.(ts|tsx)$/,
      include: paths.appSrc,
      use: [
        // babel-loader
        {
          loader: 'babel-loader',
          options: {
            plugins: [
              [
                '@babel/plugin-transform-react-jsx',
                {
                  pragma: 'h',
                  pragmaFrag: 'Fragment',
                  runtime: 'classic',
                },
              ],
            ],
          },
        },
        {
          loader: require.resolve('ts-loader'),
          options: {
            configFile: paths.appTsConfig,
            transpileOnly: true
          }
        },
      ],
    });

    config.resolve.extensions.push('.ts', '.tsx');
    config.resolve.plugins = [new TsconfigPathsPlugin()];
    config.resolve.alias =       {
        react: 'preact/compat',
        'react-dom': 'preact/compat',
        '@': path.resolve(paths.appSrc, 'ts'),
      };


    return config;
  }
};
