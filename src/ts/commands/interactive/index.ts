import { DescriptorsType } from "@/commands/type";
import * as decisionCancel from "./decision/cancel";
import * as decisionFinish from "./decision/finish";
import * as decisionCursorDown from "./decision/cursor-down";
import * as decisionCursorUp from "./decision/cursor-up";
import * as filerDeleteItems from "./filer/delete-items";
import * as filerCopyItems from "./filer/copy-items";
import * as finderClose from "./finder/close";
import * as finderConfirm from "./finder/confirm";
import * as finderOpen from "./finder/open";
import * as filerToggleMark from "./filer/toggle-mark";
import * as filerUpDirectory from "./filer/up-directory";
import * as filerOpenNode from "./filer/open-node";
import * as keymapReload from "./keymap/reload";
import * as filerReloadAll from "./filer/reload-all";
import * as filerMoveLocation from "./filer/move-location";
import * as filerCursorUp from "./filer/cursor-up";
import * as filerCursorDown from "./filer/cursor-down";
import * as filerChangeSide from "./filer/change-side";
import * as completerCursorDown from "./completer/cursor-down";
import * as completerCursorUp from "./completer/cursor-up";
import * as filerMoveItems from "./filer/move-items";
import { Type } from "../command-resolver";

// prettier-ignore
export const descriptors = {
  decisionCancel: decisionCancel.descriptor,
  decisionFinish: decisionFinish.descriptor,
  decisionCursorDown: decisionCursorDown.descriptor,
  decisionCursorUp: decisionCursorUp.descriptor,
  filerDeleteItems: filerDeleteItems.descriptor,
  filerCopyItems: filerCopyItems.descriptor,
  finderClose: finderClose.descriptor,
  finderConfirm: finderConfirm.descriptor,
  finderOpen: finderOpen.descriptor,
  filerToggleMark: filerToggleMark.descriptor,
  filerUpDirectory: filerUpDirectory.descriptor,
  filerOpenNode: filerOpenNode.descriptor,
  keymapReload: keymapReload.descriptor,
  filerReloadAll: filerReloadAll.descriptor,
  filerMoveLocation: filerMoveLocation.descriptor,
  filerCursorUp: filerCursorUp.descriptor,
  filerCursorDown: filerCursorDown.descriptor,
  filerChangeSide: filerChangeSide.descriptor,
  completerCursorDown: completerCursorDown.descriptor,
  completerCursorUp: completerCursorUp.descriptor,
  filerMoveItems: filerMoveItems.descriptor
} as const;

export type Descriptors = DescriptorsType<typeof descriptors>;

/**
 * register commands
 */
// prettier-ignore
export const registerToResolver = function registerToResolver(resolver: Type) {
  resolver.register({descriptor: decisionCancel.descriptor, factory: decisionCancel.createCommand});
  resolver.register({descriptor: decisionFinish.descriptor, factory: decisionFinish.createCommand});
  resolver.register({descriptor: decisionCursorDown.descriptor, factory: decisionCursorDown.createCommand});
  resolver.register({descriptor: decisionCursorUp.descriptor, factory: decisionCursorUp.createCommand});
  resolver.register({descriptor: filerDeleteItems.descriptor, factory: filerDeleteItems.createCommand});
  resolver.register({descriptor: filerCopyItems.descriptor, factory: filerCopyItems.createCommand});
  resolver.register({descriptor: finderClose.descriptor, factory: finderClose.createCommand});
  resolver.register({descriptor: finderConfirm.descriptor, factory: finderConfirm.createCommand});
  resolver.register({descriptor: finderOpen.descriptor, factory: finderOpen.createCommand});
  resolver.register({descriptor: filerToggleMark.descriptor, factory: filerToggleMark.createCommand});
  resolver.register({descriptor: filerUpDirectory.descriptor, factory: filerUpDirectory.createCommand});
  resolver.register({descriptor: filerOpenNode.descriptor, factory: filerOpenNode.createCommand});
  resolver.register({descriptor: keymapReload.descriptor, factory: keymapReload.createCommand});
  resolver.register({descriptor: filerReloadAll.descriptor, factory: filerReloadAll.createCommand});
  resolver.register({descriptor: filerMoveLocation.descriptor, factory: filerMoveLocation.createCommand});
  resolver.register({descriptor: filerCursorUp.descriptor, factory: filerCursorUp.createCommand});
  resolver.register({descriptor: filerCursorDown.descriptor, factory: filerCursorDown.createCommand});
  resolver.register({descriptor: filerChangeSide.descriptor, factory: filerChangeSide.createCommand});
  resolver.register({descriptor: completerCursorDown.descriptor, factory: completerCursorDown.createCommand});
  resolver.register({descriptor: completerCursorUp.descriptor, factory: completerCursorUp.createCommand});
  resolver.register({descriptor: filerMoveItems.descriptor, factory: filerMoveItems.createCommand});
};
