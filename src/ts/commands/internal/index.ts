import { DescriptorsType } from "@/commands/type";
import * as themeGet from "./theme/get";
import * as themeUpload from "./theme/upload";
import * as configurationUpdateAll from "./configuration/update-all";
import * as configurationUpdate from "./configuration/update";
import * as configurationSelectSection from "./configuration/select-section";
import * as completerUpdateCandidates from "./completer/update-candidates";
import * as completerInitialize from "./completer/initialize";
import * as completerComplete from "./completer/complete";
import * as filerUpdateFileWindow from "./filer/update-file-window";
import * as decisionReset from "./decision/reset";
import * as decisionRequest from "./decision/request";
import * as decisionUpdateNewName from "./decision/update-new-name";
import * as configurationInitialize from "./configuration/initialize";
import * as keymapRemoveContext from "./keymap/remove-context";
import * as keymapAddContext from "./keymap/add-context";
import * as filerUpdate from "./filer/update";
import * as filerInitialize from "./filer/initialize";
import { Type } from "../command-resolver";

// prettier-ignore
export const descriptors = {
  themeGet: themeGet.descriptor,
  themeUpload: themeUpload.descriptor,
  configurationUpdateAll: configurationUpdateAll.descriptor,
  configurationUpdate: configurationUpdate.descriptor,
  configurationSelectSection: configurationSelectSection.descriptor,
  completerUpdateCandidates: completerUpdateCandidates.descriptor,
  completerInitialize: completerInitialize.descriptor,
  completerComplete: completerComplete.descriptor,
  filerUpdateFileWindow: filerUpdateFileWindow.descriptor,
  decisionReset: decisionReset.descriptor,
  decisionRequest: decisionRequest.descriptor,
  decisionUpdateNewName: decisionUpdateNewName.descriptor,
  configurationInitialize: configurationInitialize.descriptor,
  keymapRemoveContext: keymapRemoveContext.descriptor,
  keymapAddContext: keymapAddContext.descriptor,
  filerUpdate: filerUpdate.descriptor,
  filerInitialize: filerInitialize.descriptor,
} as const;

export type Descriptors = DescriptorsType<typeof descriptors>;

/**
 * register commands
 */
// prettier-ignore
export const registerToResolver = function registerToResolver(resolver: Type) {
  resolver.register({descriptor: themeGet.descriptor, factory: themeGet.createCommand});
  resolver.register({descriptor: themeUpload.descriptor, factory: themeUpload.createCommand});
  resolver.register({descriptor: configurationUpdateAll.descriptor, factory: configurationUpdateAll.createCommand});
  resolver.register({descriptor: configurationUpdate.descriptor, factory: configurationUpdate.createCommand});
  resolver.register({descriptor: configurationSelectSection.descriptor, factory: configurationSelectSection.createCommand});
  resolver.register({descriptor: completerUpdateCandidates.descriptor, factory: completerUpdateCandidates.createCommand});
  resolver.register({descriptor: completerInitialize.descriptor, factory: completerInitialize.createCommand});
  resolver.register({descriptor: completerComplete.descriptor, factory: completerComplete.createCommand});
  resolver.register({descriptor: filerUpdateFileWindow.descriptor, factory: filerUpdateFileWindow.createCommand});
  resolver.register({descriptor: decisionReset.descriptor, factory: decisionReset.createCommand});
  resolver.register({descriptor: decisionRequest.descriptor, factory: decisionRequest.createCommand});
  resolver.register({descriptor: decisionUpdateNewName.descriptor, factory: decisionUpdateNewName.createCommand});
  resolver.register({descriptor: configurationInitialize.descriptor, factory: configurationInitialize.createCommand});
  resolver.register({descriptor: keymapRemoveContext.descriptor, factory: keymapRemoveContext.createCommand});
  resolver.register({descriptor: keymapAddContext.descriptor, factory: keymapAddContext.createCommand});
  resolver.register({descriptor: filerUpdate.descriptor, factory: filerUpdate.createCommand});
  resolver.register({descriptor: filerInitialize.descriptor, factory: filerInitialize.createCommand});
};
