import * as filerChangePaneSide from "./filer-change-pane-side";
import * as filerEnterDirectory from "./filer-enter-directory";
import * as filerMoveCursorDown from "./filer-move-cursor-down";
import * as filerMoveCursorUp from "./filer-move-cursor-up";
import * as filerMoveParent from "./filer-move-parent";
import * as filerToggleMark from "./filer-toggle-mark";
import * as keymapReload from "./keymap-reload";

import { CommandRegistrar } from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [
    filerChangePaneSide,
    filerEnterDirectory,
    filerMoveCursorDown,
    filerMoveCursorUp,
    filerMoveParent,
    filerToggleMark,
    keymapReload,
  ];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
};
