import * as Rx from 'rx';
import keyMirror from 'keymirror';
import {IPC_KEYS} from 'sxfiler/common/constants';
import {RendererIPC} from 'sxfiler/renderer/renderer-ipc';

interface Actions {
  QUIT_APPLICATION: string;
}

/**
 * Actions for directory.
 * @type {object}
 */
export const ACTIONS: Actions = keyMirror({
  QUIT_APPLICATION: null
});

/**
 * Quit the current application.
 * 
 * @return {function(ipc:RendererIPC):Rx.Observable} An observable sequence resulting ipc call
 */
export function quitApplication(): (ipc: RendererIPC) => Rx.Observable<any> {
  'use strict';
  return (ipc: RendererIPC) => ipc.send(IPC_KEYS.QUIT_APPLICATION);
}
