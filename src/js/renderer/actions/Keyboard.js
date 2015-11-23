import Rx from 'rx';
import keyMirror from 'keymirror';

/**
 * Actions of keyboard.
 * @type {object}
 */
export const ACTIONS = keyMirror({
  MOVE_CURSOR:null,
  CHANGE_PANE: null,
  CHANGE_DIRECTORY: null,
  QUIT_APPLICATION: null
});

/**
 * A observable publish actions for keyboard actions
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
 * Publish event to up cursor.
 *
 * @param {string} pane Specified pane to request directory
 */
export function upCursor(pane) {
  subject.onNext({key: ACTIONS.MOVE_CURSOR, pane, direction: -1});
}

/**
 * Publish event to down cursor.
 *
 * @param {string} pane Pane
 */
export function downCursor(pane) {
  subject.onNext({key: ACTIONS.MOVE_CURSOR, pane, direction: 1});
}

/**
 * Publish event to change current pane.
 *
 * @param {string} pane Current focusd pane
 */
export function changePane(pane) {
  subject.onNext({key: ACTIONS.CHANGE_PANE, pane});
}

/**
 * Publish event to change directory.
 *
 * @param {string} path A path to change directory 
 * @param {string} pane A pane to change directory
 */
export function changeDirectory(path, pane) {
  subject.onNext({key: ACTIONS.CHANGE_DIRECTORY, path, pane});
}

/**
 * Publish event to quit application.
 */
export function quitApplication(path, pane) {
  subject.onNext({key: ACTIONS.QUIT_APPLICATION});
}
