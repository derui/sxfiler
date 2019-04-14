
import * as filerChangePaneSide from "./filer-change-pane-side"
import * as filerMoveCursorDown from "./filer-move-cursor-down"
import * as filerMoveCursorUp from "./filer-move-cursor-up"
import * as filerMoveParent from "./filer-move-parent"

import {CommandRegistrar} from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [filerChangePaneSide,filerMoveCursorDown,filerMoveCursorUp,filerMoveParent];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
}
