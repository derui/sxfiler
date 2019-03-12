module.exports = {
  plugins: ["@typescript-eslint"],
  parser: '@typescript-eslint/parser',
  extends:  [
    'plugin:prettier/recommended',
    "plugin:react/recommended",
  ],
  env: {
    es6: true,
    browser: true,
  },
  parserOptions:  {
    ecmaVersion:  2018,  // Allows for the parsing of modern ECMAScript features
    sourceType:  'module',  // Allows for the use of imports
    ecmaFeatures:  {
      jsx: true,  // Allows for the parsing of JSX
    },
    project: "./tsconfig.json",
  },
  rules: {

    "react/jsx-uses-vars": ["warn", {
      "extensions": [".tsx"]
    }],
    "@typescript-eslint/no-unused-vars": "error",
    "@typescript-eslint/no-unnecessary-type-assertion": "error",
    "prettier/prettier": ['error'],
  },
  settings:  {
    react:  {
      version:  'detect',  // Tells eslint-plugin-react to automatically detect the version of React to use
    },
  },
}
