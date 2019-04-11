
import * as filerMoveCursorDown from "./filer-move-cursor-down"
import * as filerMoveCursorUp from "./filer-move-cursor-up"

import {CommandRegistrar} from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [filerMoveCursorDown,filerMoveCursorUp];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
}
