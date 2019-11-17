const path = require('path');
const glob = require('glob');
const fs = require('fs');

const OUTPUT_FILE_NAME = path.resolve('src/ts/commands/builtins/index.ts');
const generateCommandModule = modules => {
  const importList = modules.map(([path, module]) => `import * as ${module} from "./${path}"`).join('\n');

  const moduleList = modules.map(([_, m]) => m).join(',');

  return `
${importList}

import {CommandRegistrar} from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [${moduleList}];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
}
`;
};

const kebabToCamel = str => {
  str = str.charAt(0).toLowerCase() + str.slice(1);
  return str.replace(/[-_](.)/g, (match, group1) => group1.toUpperCase());
};

glob('src/ts/commands/builtins/**/*.ts', (er, files) => {
  const extractModuleName = v => path.basename(v, path.extname(v));

  const moduleNames = files
    .filter(v => !v.includes('.test.'))
    .filter(v => !v.includes('index.ts'))
    .map(extractModuleName)
    .map(v => [v, kebabToCamel(v)]);

  fs.writeFileSync(OUTPUT_FILE_NAME, generateCommandModule(moduleNames), {
    mode: 0o644,
  });
});
