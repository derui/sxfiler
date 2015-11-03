import keyMirror from 'keymirror';

/**
 * Event names for IPC.
 *
 * Each event name have 'request' or 'finish' prefix.
 *
 * @type {object}
 */
export const IPCKeys = keyMirror({
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
 * Pane typess
 *
 * @type {object}
 */
export const Pane = keyMirror({
  LEFT: null,
  RIGHT: null,
  BOTTOM: null
});
