import R from 'ramda';
import Path from 'path';
import Promise from 'bluebird';

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
   * @param {fs} fs node-fs module using by this method.
   * @param {string} path A path of directory to get file list
   * @return {Promise} Promise for asynchronous getting file informations. 
   */
  static getFileInformationsOfDirectory(fs, path) {
    let current = Promise.resolve();
    return Promise.resolve().then(() => Promise.resolve(fs.statSync(path))).then((stat) => {
      if (!stat.isDirectory()) {
        return Promise.reject(new Error(`${path} is not directory`));
      }

      return Promise.resolve().then(() => Promise.resolve(fs.readdirSync(path)));
    }).map((fpath) => {
      current = current.then(() => Promise.resolve([fpath, fs.statSync(Path.join(path, fpath))]));
      return current;
    }).reduce((memo, [fpath, stat]) => R.merge(memo, R.objOf(fpath, stat)), {});
  }

}
