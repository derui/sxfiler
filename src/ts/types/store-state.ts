import * as FileList from "./store-state/file-list";
import * as Keymap from "./store-state/keymap";
import * as Notification from "./store-state/notification";
import UIContext from "./ui-context";

export interface ConfigState {}

export interface StoreState {
  context: UIContext;
  config: ConfigState;
  fileList: FileList.State;
  keymap: Keymap.State;
  notification: Notification.State;
}
