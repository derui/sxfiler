// define default commands for operation to pane
import * as Rx from 'rx';
import * as types from '../types';
import {PANE} from 'sxfiler/common/constants';
import {GlobalState} from 'sxfiler/renderer/types';
import uuid from 'uuid';

// a command to move cursor up on current pane
export const upCursor: types.Command = {
  name: 'Pane.upCursor',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    let paneInfo = globalState.paneInfo[globalState.currentPane];
    let pane = globalState.currentPane;

    return actions.paneOperation.moveCursor(pane, -1);
  }
};

export const downCursor: types.Command = {
  name: 'Pane.downCursor',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    let paneInfo = globalState.paneInfo[globalState.currentPane];
    let pane = globalState.currentPane;

    return actions.paneOperation.moveCursor(pane, 1);
  }
};

export const changePane: types.Command = {
  name: 'Pane.changePane',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    let pane = globalState.currentPane;

    return actions.paneOperation.changeCurrentPane(pane === PANE.LEFT ? PANE.RIGHT : PANE.LEFT);
  }
};
