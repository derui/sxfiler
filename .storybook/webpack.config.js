const _RuleSet = require("webpack/lib/RuleSet");

const TSDocgenPlugin = require("react-docgen-typescript-webpack-plugin");

// functions from cra-config.js of storybook
const cssExtensions = ['.css', '.scss', '.sass'];
const cssModuleExtensions = ['.module.css', '.module.scss', '.module.sass'];

const getRules = extensions => rules => rules.reduce((craRules, rule) => {
  // If at least one extension satisfies the rule test, the rule is one
  // we want to extract
  if (rule.test && extensions.some((0, _RuleSet.normalizeCondition)(rule.test))) {
    // If the base test is for extensions, return early
    return craRules.concat(rule);
  } // Get any rules contained in rule.oneOf


  if (!rule.test && rule.oneOf) {
    craRules.push(...getRules(extensions)(rule.oneOf));
  } // Get any rules contained in rule.rules


  if (!rule.test && rule.rules) {
    craRules.push(...getRules(extensions)(rule.rules));
  }

  return craRules;
}, []);

const getStyleRules = getRules(cssExtensions.concat(cssModuleExtensions));

module.exports = ({config, mode}) => {
  craConfig = require("../config/webpack.config.js")(config.mode);
  const styleRules = getStyleRules(craConfig.module.rules)
  config = Object.assign(
    config, {
      module: Object.assign({}, config.module, {
        rules: [...config.module.rules, ...styleRules]
      }),
    });

  config.module.rules.push({
    test: /\.(ts|tsx)$/,
    loader: require.resolve('babel-loader'),
    options: {
      presets: [require.resolve('babel-preset-react-app')]
    }
  });

  config.plugins.push(new TSDocgenPlugin());
  config.resolve.extensions.push('.ts', '.tsx');

  return config;
};
