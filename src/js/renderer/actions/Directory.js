import R from 'ramda';
import Rx from 'rx';
import keyMirror from 'keymirror';

/**
 * Actions for directory.
 * @type {object}
 */
export const ACTIONS = keyMirror({
  REQUEST_DIRECTORY: null,
  RECEIVE_DIRECTORY: null
});

/**
 * A observable publish actions for directory
 * 
 * @type {Rx.Observable}
 */
export let subject = new Rx.ReplaySubject(1);

/**
 * Unsubscribe all observers, and renewal subject for actions
 */
export function dispose() {
  subject.dispose();
  subject = new Rx.ReplaySubject(1);
}

/**
 * Publish event for request directory.
 *
 * @param {string} path New path for display to left-side window
 * @param {string} pane Specified pane to request directory
 */
export function requestDirectory(path, pane) {
  subject.onNext({key: ACTIONS.REQUEST_DIRECTORY, path, pane});
}

/**
 * Publish event when receive directory information from ipc.
 *
 * @param {string} path An absolute path of directory
 * @param {array[object]} fileList file information list
 * @param {string} pane Pane
 */
export function receiveDirectory(path, fileList, pane) {
  subject.onNext({key: ACTIONS.RECEIVE_DIRECTORY, path, fileList, pane});
}
