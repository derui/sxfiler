import * as Config from "./store-state/config";
import * as FileList from "./store-state/file-list";
import * as Keymap from "./store-state/keymap";
import * as Notification from "./store-state/notification";
import UIContext from "./ui-context";

export interface StoreState {
  context: UIContext;
  config: Config.State;
  fileList: FileList.State;
  keymap: Keymap.State;
  notification: Notification.State;
}
