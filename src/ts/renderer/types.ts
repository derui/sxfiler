import * as types from 'sxfiler/common/types';

export interface PaneInfo {
  currentPath: string;
  fileList: types.File[];
  selected: number;
  pane: string;
}

export interface PaneInfos {
  [pane: string]: PaneInfo;
}

// state of global.
export interface GlobalState {
  // state for each pane
  paneInfo: PaneInfos;

  // current located pane
  currentPane: string;
}
