import {IPC_KEYS} from '../common/constants';
import FileListUtil from './file-list-util';
import * as Path from 'path';
import * as fs from 'fs';
import * as types from '../common/types';

interface Event {
  sender: {
    send: (event: string, ...args: any[]) => void;
  };
}

/**
 * IPC event management for Main process.
 *
 * This class provide only event handlers for request event in {IPC_KEYS}.
 * User must not instance greater than two times, so user only initialize in Main class
 * for Main process.
 */
export default class MainIPC {

  private _ipc: NodeJS.EventEmitter;
  private _fs: typeof fs;

  /**
   * Construct MainIPC
   *
   * @param {!IPC} ipc ipc module using this
   * @param {!fs} fs File System module
   */
  constructor(ipc: NodeJS.EventEmitter, _fs: typeof fs) {
    this._ipc = ipc;
    this._fs = fs;

    this._ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, this._onRequestFilesInDirectory.bind(this));
  }

  /**
   * @private
   * @param {event} ev Event object from ipc event emitter
   * @param {string} path What path for directory target to get files
   * @param {string} uuid The unique event id between this and sender via UUID.
   */
  private _onRequestFilesInDirectory(ev: Event, path: string, uuid: string): void {
    if (!path) {
      ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, new Error('Invalid argument'), null);
      return;
    }

    let absolute = Path.resolve(path);

    FileListUtil.getFileInformationsOfDirectory(this._fs, absolute).then((files: types.FileStats) => {
      ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, null, absolute, files, uuid);
    }).catch((err: Error) => {
      ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, err, absolute, [], uuid);
    });
  }
}
