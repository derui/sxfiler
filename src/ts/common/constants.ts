import keyMirror from 'keymirror';

interface IPCKeys {
  REQUEST_FILES_IN_DIRECTORY: string;
  FINISH_FILES_IN_DIRECTORY: string;
  QUIT_APPLICATION: string;
}

/**
 * Event names for IPC.
 *
 * Each event name have 'request' or 'finish' prefix.
 *
 * @type {object}
 */
export const IPC_KEYS: IPCKeys = keyMirror({
  /**
   * Event name when request file informations in the directory.
   * @type {string}
   */
  REQUEST_FILES_IN_DIRECTORY: null,

  /**
   * Event name when finish getting file informations in the directory
   * @type {string}
   */
  FINISH_FILES_IN_DIRECTORY: null,

  /**
   * Event sending when user request to quit application
   * @type {string}
   */
  QUIT_APPLICATION: null
});

/**
 * Pane types
 *
 * @type {Pane}
 */
export const PANE: {LEFT: string; RIGHT: string; BOTTOM: string} = keyMirror({
  BOTTOM: null,
  LEFT: null,
  RIGHT: null
});
