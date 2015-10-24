import {IPCKeys} from 'sxfiler/common/Constants';
import Promise from 'bluebird';
import Path from 'path';
import R from 'ramda';

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
  _onRequestFilesInDirectory(ev, [path, pane]) {
    if (!path) {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, new Error('Invalid argument'), null);
      return;
    }

    this._getFileInformationsOfDirectory(path).then((files) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, files, pane);
    }).catch((err) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, err, [], pane);
    });
  }

  /**
   * @private
   * @param {string} path A path of directory to get file list
   * @return {Promise} Promise for asynchronous getting file informations. 
   */
  _getFileInformationsOfDirectory(path) {
    let current = Promise.resolve();
    return Promise.resolve().then(() => Promise.resolve(this._fs.statSync(path))).then((stat) => {
      if (!stat.isDirectory()) {
        return Promise.reject(new Error(`${path} is not directory`));
      }

      return Promise.resolve().then(() => Promise.resolve(this._fs.readdirSync(path)));
    }).map((fpath) => {
      current = current.then(() => Promise.resolve([fpath, this._fs.statSync(Path.join(path, fpath))]));
      return current;
    }).reduce((memo, [fpath, stat]) => R.merge(memo, R.objOf(fpath, stat)), {});
  }
}
