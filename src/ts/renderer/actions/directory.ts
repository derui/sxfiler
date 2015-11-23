import * as Rx from 'rx';
import keyMirror from 'keymirror';
import {IPC_KEYS} from 'sxfiler/common/constants';
import {RendererIPC} from 'sxfiler/renderer/renderer-ipc';

interface Actions {
  REQUEST_DIRECTORY: string;
  RECEIVE_DIRECTORY: string;
}

namespace action.directory {
  'use strict';
  // an interface of receiveDirectory action
  export interface Receiver {
    key: string;
    path: string;
    pane: string;
    fileList: any[];
  }

  // an interface of requestDirectory action
  export interface Requester {
    key: string;
    path: string;
    pane: string;
  }
}

/**
 * Actions for directory.
 * @type {object}
 */
export const ACTIONS: Actions = keyMirror({
  REQUEST_DIRECTORY: null,
  RECEIVE_DIRECTORY: null
});

/**
 * Publish event for request directory.
 *
 * @param {string} path New path for display to left-side window
 * @param {string} pane Specified pane to request directory
 * @return {Rx.Observable} An observeable sequence return just an action
 */
function requestDirectory(path: string, pane: string): Rx.Observable<action.directory.Requester> {
  'use strict';
  return Rx.Observable.just({key: ACTIONS.REQUEST_DIRECTORY, path, pane});
}

/**
 * Publish event when receive directory information from ipc.
 *
 * @param {string} path An absolute path of directory
 * @param {string} pane Specified pane to request directory
 * @param {array[object]} fileList file information list
 * @return {Rx.Observable}
 */
function receiveDirectory(path: string, pane: string, fileList: any[]): Rx.Observable<action.directory.Receiver> {
  'use strict';
  return Rx.Observable.just({key: ACTIONS.RECEIVE_DIRECTORY, path, pane, fileList});
}

/**
 * Get a file list in directory.
 * 
 * @param {string} path The path of directory that request file list in it
 * @return {function(ipc:RendererIPC):Rx.Observable} An observable sequence resulting ipc call
 */
export function getDirectory(path: string, pane: string): (ipc: RendererIPC) => Rx.Observable<any> {
  'use strict';
  return (ipc: RendererIPC) => {
    let event = ipc.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, path)
          .flatMap(([err, path, fileList]) => {
            if (err) {
              return Rx.Observable.throw(new Error(err));
            }
            return receiveDirectory(path, pane, fileList);
          });
    return Rx.Observable.concat(requestDirectory(path, pane), event);
  };
}
