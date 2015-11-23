// define default commands for directory 
import * as Rx from 'rx';
import * as types from '../types';
import {GlobalState} from 'sxfiler/renderer/types';
import uuid from 'uuid';
import * as path from 'path';
import Util from 'sxfiler/renderer/utils/file-type-util';

// a command for upstair directory structure on current pane.
export const upDirectory: types.Command = {
  name: 'Pane.upDirectory',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    let paneInfo = globalState.paneInfo[globalState.currentPane];
    let nextPath = path.join(paneInfo.currentPath, '..');
    let pane = globalState.currentPane;

    return actions.directory.getDirectory(nextPath, pane);
  }
};

// a command for upstair directory structure on current pane.
export const downDirectory: types.Command = {
  name: 'Pane.downDirectory',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    let paneInfo = globalState.paneInfo[globalState.currentPane];
    let selected = paneInfo.fileList[paneInfo.selected];
    let nextPath = path.join(paneInfo.currentPath, selected.filename);
    let pane = globalState.currentPane;

    // return empty if selected item is not directory
    if (!Util.isDirectory(selected.stat.mode)) {
      return Rx.Observable.empty<any>();
    }

    return actions.directory.getDirectory(nextPath, pane);
  }
};
