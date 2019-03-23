import * as Config from "./store-state/config";
import * as FileList from "./store-state/file-list";
import * as Keymap from "./store-state/keymap";
import * as Notification from "./store-state/notification";
import UIContext from "./ui-context";

export type StoreState = {
  context: UIContext;
  config: Config.State;
  fileList: FileList.State;
  keymap: Keymap.State;
  notification: Notification.State;
};

// get empty state
export function empty(): StoreState {
  return {
    context: UIContext.OnFileTree,
    config: Config.empty(),
    fileList: FileList.empty(),
    keymap: Keymap.empty(),
    notification: Notification.empty(),
  };
}
