import * as bookmarkToggle from "./bookmark-toggle";
import * as completerClose from "./completer-close";
import * as completerCursorDown from "./completer-cursor-down";
import * as completerCursorUp from "./completer-cursor-up";
import * as filerChangePaneSide from "./filer-change-pane-side";
import * as filerCopyItems from "./filer-copy-items";
import * as filerDeleteItems from "./filer-delete-items";
import * as filerEnterDirectory from "./filer-enter-directory";
import * as filerMoveCursorDown from "./filer-move-cursor-down";
import * as filerMoveCursorUp from "./filer-move-cursor-up";
import * as filerMoveItems from "./filer-move-items";
import * as filerMoveParent from "./filer-move-parent";
import * as filerToggleMark from "./filer-toggle-mark";
import * as finderClose from "./finder-close";
import * as finderCursorDown from "./finder-cursor-down";
import * as finderCursorUp from "./finder-cursor-up";
import * as finderOpen from "./finder-open";
import * as finderSelect from "./finder-select";
import * as historyClose from "./history-close";
import * as historyCursorDown from "./history-cursor-down";
import * as historyCursorUp from "./history-cursor-up";
import * as historyOpen from "./history-open";
import * as historySelect from "./history-select";
import * as keymapReload from "./keymap-reload";
import * as taskSelectNextReply from "./task-select-next-reply";
import * as taskSelectPreviousReply from "./task-select-previous-reply";

import { CommandRegistrar } from "../command-registrar";

export const registAllCommand = (registrar: CommandRegistrar): CommandRegistrar => {
  const modules = [
    bookmarkToggle,
    completerClose,
    completerCursorDown,
    completerCursorUp,
    filerChangePaneSide,
    filerCopyItems,
    filerDeleteItems,
    filerEnterDirectory,
    filerMoveCursorDown,
    filerMoveCursorUp,
    filerMoveItems,
    filerMoveParent,
    filerToggleMark,
    finderClose,
    finderCursorDown,
    finderCursorUp,
    finderOpen,
    finderSelect,
    historyClose,
    historyCursorDown,
    historyCursorUp,
    historyOpen,
    historySelect,
    keymapReload,
    taskSelectNextReply,
    taskSelectPreviousReply,
  ];

  return modules.reduce((accum, module) => module.registCommand(accum), registrar);
};
