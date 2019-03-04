module.exports = {
  // parserに'vue-eslint-parser'を指定し、'@typescript-eslint/parser'はparserOptionsに指定する
  parser: '@typescript-eslint/parser',
  extends:  [
    'plugin:@typescript-eslint/recommended',
    'plugin:prettier/recommended',
    'plugin:react/recommended',
  ],
  parserOptions:  {
    ecmaVersion:  2018,  // Allows for the parsing of modern ECMAScript features
    sourceType:  'module',  // Allows for the use of imports
    ecmaFeatures:  {
      jsx: true,  // Allows for the parsing of JSX
    },
  },
  // Place to specify ESLint rules. Can be used to overwrite rules specified from the extended configs
  // e.g. "@typescript-eslint/explicit-function-return-type": "off",
  rules: {
    "@typescript-eslint/interface-name": false,
    "@typescript-eslint/no-empty-interface": false,
    "@typescript-eslint/object-literal-sort-keys": false,
    "@typescript-eslint/object-literal-shorthand": false,
    "@typescript-eslint/variable-name": [true, "ban-keywords", "check-format", "allow-leading-underscore", "allow-pascal-case"],

    'prettier/prettier': ['error']
  },
  settings:  {
    react:  {
      version:  'detect',  // Tells eslint-plugin-react to automatically detect the version of React to use
    },
  },
}
