import * as FileList from "./store-state/file-list";

export interface ConfigState {}

export interface StoreState {
  config: ConfigState;
  fileList: FileList.State;
}

export { State as FileListState } from "./store-state/file-list";
