import * as filerChangePaneSide from "./filer-change-pane-side";
import * as filerDeleteItems from "./filer-delete-items";
import * as filerEnterDirectory from "./filer-enter-directory";
import * as filerMoveCursorDown from "./filer-move-cursor-down";
import * as filerMoveCursorUp from "./filer-move-cursor-up";
import * as filerMoveItems from "./filer-move-items";
import * as filerMoveParent from "./filer-move-parent";
import * as filerToggleMark from "./filer-toggle-mark";
import * as keymapReload from "./keymap-reload";
import * as taskSelectNextReply from "./task-select-next-reply";
import * as taskSelectPreviousReply from "./task-select-previous-reply";

import { CommandRegistrar } from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [
    filerChangePaneSide,
    filerDeleteItems,
    filerEnterDirectory,
    filerMoveCursorDown,
    filerMoveCursorUp,
    filerMoveItems,
    filerMoveParent,
    filerToggleMark,
    keymapReload,
    taskSelectNextReply,
    taskSelectPreviousReply,
  ];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
};
