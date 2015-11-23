/**
 * FileType in a stats mode returning fs.stats.
 *
 * Each types has charactor to display mode string.
 * 
 * @type {object}
 */

interface Types {
  DIRECTORY: string;
  NORMAL: string;
  SYMLINK: string;
}

export const TYPES: Types = {
  DIRECTORY: 'd',
  NORMAL: '-',
  SYMLINK: 'l'
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
  public static modeToFileType(mode: number): string {
    const directory = 0o40000;
    const symlink = 0o120000;

    if (mode & directory) {
      return TYPES.DIRECTORY;
    } else if ((mode & symlink) === symlink) {
      return TYPES.SYMLINK;
    } else {
      return TYPES.NORMAL;
    }
  }

  /**
   * Get given mode having Directory file type.
   *
   * @param {number} mode the mode to check directory or not
   * @return {boolean} the mode is directory or not
   */
  public static isDirectory(mode: number): boolean {
    return this.modeToFileType(mode) === TYPES.DIRECTORY;
  }

  /**
   * Get given mode having Symlink file type.
   *
   * @param {number} mode the mode to check symlink or not
   * @return {boolean} the mode is symlink or not
   */
  public static isSymlink(mode: number): boolean {
    return this.modeToFileType(mode) === TYPES.SYMLINK;
  }

  /**
   * Get given mode having File file type.
   *
   * @param {number} mode the mode to check file or not
   * @return {boolean} the mode is file or not
   */
  public static isFile(mode: number): boolean {
    return this.modeToFileType(mode) === TYPES.NORMAL;
  }

}

