import * as FileList from "./store-state/file-list";
import * as Keymap from "./store-state/keymap";
import UIContext from "./ui-context";

export interface ConfigState {}

export interface StoreState {
  context: UIContext;
  config: ConfigState;
  fileList: FileList.State;
  keymap: Keymap.State;
}

export { State as FileListState } from "./store-state/file-list";
export { State as KeymapState } from "./store-state/keymap";
