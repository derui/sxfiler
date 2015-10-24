/**
 * FileType in a stats mode returning fs.stats.
 *
 * Each types has charactor to display mode string.
 * 
 * @type {object}
 */
export let Types = {
  DIRECTORY: 'd',
  SYMLINK: 'l',
  NORMAL: '-'
};

/**
 * Providing utility function to filetype for mode.
 */
export default class FileTypeUtil {

  /**
   * Get file type as string in the mode of the file.
   * 
   * @param {number} mode mode of the file
   * @return {string} file type
   */
  static modeToFileType(mode) {
    const directory = 0o40000;
    const symlink = 0o120000;

    if (mode & directory) {
      return Types.DIRECTORY;
    } else if ((mode & symlink) === symlink) {
      return Types.SYMLINK;
    } else {
      return Types.NORMAL;
    }
  }

  /**
   * Get given mode having Directory file type.
   *
   * @param {number} mode the mode to check directory or not
   * @return {boolean} the mode is directory or not
   */
  static isDirectory(mode) {
    return this.modeToFileType(mode) === Types.DIRECTORY;
  }

  /**
   * Get given mode having Symlink file type.
   *
   * @param {number} mode the mode to check symlink or not
   * @return {boolean} the mode is symlink or not
   */
  static isSymlink(mode) {
    return this.modeToFileType(mode) === Types.SYMLINK;
  }

  /**
   * Get given mode having File file type.
   *
   * @param {number} mode the mode to check file or not
   * @return {boolean} the mode is file or not
   */
  static isFile(mode) {
    return this.modeToFileType(mode) === Types.NORMAL;
  }

}

