module.exports = {
  plugins: ["@typescript-eslint"],
  parser: '@typescript-eslint/parser',
  extends:  [
    'plugin:prettier/recommended',
    "plugin:react/recommended",
    "plugin:import/errors",
    "plugin:import/warnings",
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
    "react/prop-types": false,
    "import/no-unresolved": false,
    "react/jsx-uses-vars": ["warn", {
      "extensions": [".tsx"]
    }],
    "@typescript-eslint/no-unused-vars": "error",
    "@typescript-eslint/no-unnecessary-type-assertion": "error",
    "prettier/prettier": ['error'],
  },
  settings:  {
    "import/parsers": {
      "@typescript-eslint/parser": [".ts", ".tsx"]
    },
    "import/resolver": {
      node: {
        extensions:  [".ts", ".tsx", ".js"]
      }
    },
    react:  {
      version:  'detect',  // Tells eslint-plugin-react to automatically detect the version of React to use
    },
  },
}
