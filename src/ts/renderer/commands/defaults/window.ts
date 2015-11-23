// define default commands for window 
import * as Rx from 'rx';
import * as types from '../types';
import {GlobalState} from 'sxfiler/renderer/types';
import uuid from 'uuid';

// a command for upstair directory structure on current pane.
export const quitApplication: types.Command = {
  name: 'Window.quitApplication',
  id: uuid.v4(),
  command: (globalState: GlobalState, actions: any): Rx.Observable<any> => {
    return actions.window.quitApplication();
  }
};
