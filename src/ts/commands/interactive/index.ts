import { DescriptorsType } from "@/commands/type";
import * as filerToggleMark from "./filer/toggle-mark";
import * as filerUpDirectory from "./filer/up-directory";
import * as filerOpenNode from "./filer/open-node";
import * as keymapGet from "./keymap/get";
import * as filerReloadAll from "./filer/reload-all";
import * as filerMoveLocation from "./filer/move-location";
import * as filerCursorUp from "./filer/cursor-up";
import * as filerCursorDown from "./filer/cursor-down";
import * as filerChangeSide from "./filer/change-side";
import * as completerCursorDown from "./completer/cursor-down";
import * as completerCursorUp from "./completer/cursor-up";
import { Type } from "../command-resolver";

// prettier-ignore
export const descriptors = {
  filerToggleMark: filerToggleMark.descriptor,
  filerUpDirectory: filerUpDirectory.descriptor,
  filerOpenNode: filerOpenNode.descriptor,
  keymapGet: keymapGet.descriptor,
  filerReloadAll: filerReloadAll.descriptor,
  filerMoveLocation: filerMoveLocation.descriptor,
  filerCursorUp: filerCursorUp.descriptor,
  filerCursorDown: filerCursorDown.descriptor,
  filerChangeSide: filerChangeSide.descriptor,
  completerCursorDown: completerCursorDown.descriptor,
  completerCursorUp: completerCursorUp.descriptor,
} as const;

export type Descriptors = DescriptorsType<typeof descriptors>;

/**
 * register commands
 */
// prettier-ignore
export const registerToResolver = function registerToResolver(resolver: Type) {
  resolver.register({descriptor: filerToggleMark.descriptor, factory: filerToggleMark.createCommand});
  resolver.register({descriptor: filerUpDirectory.descriptor, factory: filerUpDirectory.createCommand});
  resolver.register({descriptor: filerOpenNode.descriptor, factory: filerOpenNode.createCommand});
  resolver.register({descriptor: keymapGet.descriptor, factory: keymapGet.createCommand});
  resolver.register({descriptor: filerReloadAll.descriptor, factory: filerReloadAll.createCommand});
  resolver.register({descriptor: filerMoveLocation.descriptor, factory: filerMoveLocation.createCommand});
  resolver.register({descriptor: filerCursorUp.descriptor, factory: filerCursorUp.createCommand});
  resolver.register({descriptor: filerCursorDown.descriptor, factory: filerCursorDown.createCommand});
  resolver.register({descriptor: filerChangeSide.descriptor, factory: filerChangeSide.createCommand});
  resolver.register({descriptor: completerCursorDown.descriptor, factory: completerCursorDown.createCommand});
  resolver.register({descriptor: completerCursorUp.descriptor, factory: completerCursorUp.createCommand});
};
