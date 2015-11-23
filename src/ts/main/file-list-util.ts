import R from 'ramda';
import Promise from 'bluebird';
import * as Path from 'path';
import * as fs from 'fs';
import * as types from 'sxfiler/common/types';
import uuid from 'uuid';

/**
 * Utility class for traversing and open file.
 */
export default class FileListUtil {

  /**
   * Get files in the directory.
   *
   * Caution, this method needs the fs module of execution environment, such as fs on node or
   * original-fs on electron.
   *
   * @param {_fs} fs node-fs module using by this method.
   * @param {string} path A path of directory to get file list
   * @return {Promise} Promise for asynchronous getting file informations. 
   */
  public static getFileInformationsOfDirectory(_fs: typeof fs, path: string): Promise<types.File[]> {
    let current = Promise.resolve([]);
    return current
      .then(() => Promise.resolve(fs.statSync(path)))
      .then((stat: fs.Stats): Promise<string[]|void> => {
        if (!stat.isDirectory()) {
          return Promise.reject(new Error(`${path} is not directory`));
        }

        return Promise.resolve(fs.readdirSync(path));
      })
      .map((fpath: string) => {
        return Promise.resolve({
          filename: fpath,
          id: uuid.v4(),
          stat: fs.statSync(Path.join(path, fpath))
        });
      });
  }

}
