import Rx from 'rx';
import keyMirror from 'keymirror';

/**
 * Actions of keyboard.
 * @type {object}
 */
export const ACTIONS = keyMirror({
  UP_CURSOR:null,
  DOWN_CURSOR: null,
  CHANGE_PANE: null
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
  subject.onNext({key: ACTIONS.UP_CURSOR, pane});
}

/**
 * Publish event to down cursor.
 *
 * @param {string} pane Pane
 */
export function downCursor(pane) {
  subject.onNext({key: ACTIONS.DOWN_CURSOR, pane});
}

/**
 * Publish event to change current pane.
 *
 * @param {string} pane Current focusd pane
 */
export function changePane(pane) {
  subject.onNext({key: ACTIONS.CHANGE_PANE, pane});
}
