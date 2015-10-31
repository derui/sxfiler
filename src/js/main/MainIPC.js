import {IPCKeys} from 'sxfiler/common/Constants';
import FileListUtil from './FileListUtil';
import Path from 'path';

/**
 * IPC event management for Main process.
 *
 * This class provide only event handlers for request event in {IPCKeys}.
 * User must not instance greater than two times, so user only initialize in Main class
 * for Main process.
 */
export default class MainIPC {

  /**
   * Construct MainIPC
   *
   * @param {!IPC} ipc ipc module using this
   * @param {!fs} fs File System module
   */
  constructor(ipc, fs) {
    this._ipc = ipc;
    this._fs = fs;

    this._ipc.on(IPCKeys.REQUEST_FILES_IN_DIRECTORY, this._onRequestFilesInDirectory.bind(this));
  }

  /**
   * @private
   * @param {event} ev Event object from ipc event emitter
   * @param {string} path What path for directory target to get files
   * @param {Pane} pane The pane what send request
   */
  _onRequestFilesInDirectory(ev, path, pane) {
    if (!path) {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, new Error('Invalid argument'), null);
      return;
    }

    let absolute = Path.resolve(path);

    FileListUtil.getFileInformationsOfDirectory(this._fs, absolute).then((files) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, absolute, files, pane);
    }).catch((err) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, err, absolute, [], pane);
    });
  }
}
