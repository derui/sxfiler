import * as Rx from 'rx';
import keyMirror from 'keymirror';

interface Actions {
  MOVE_CURSOR: string;
  CHANGE_FOCUSED_PANE: string;
}

namespace action.pane {
  'use strict';

  // an interface of moving cursor operation
  export interface Mover {
    key: string;
    pane: string;
    amount: number;
  }

  // an interface of changing current pane operation
  export interface PaneChanger {
    key: string;
    pane: string;
  }
}

/**
 * Operations for pane.
 * @type {object}
 */
export const ACTIONS: Actions = keyMirror({
  MOVE_CURSOR: null,
  CHANGE_FOCUSED_PANE: null
});

/**
 * Publish event for moving cursor on the specified pane.
 *
 * @param {PANE} pane - a pane to move cursor on it
 * @param {number} amount - amount of moving cursor
 * @return {Rx.Observable} An observeable sequence return just an action
 */
export function moveCursor(pane: string, amount: number): Rx.Observable<action.pane.Mover> {
  'use strict';
  return Rx.Observable.just({key: ACTIONS.MOVE_CURSOR, pane, amount});
}

/**
 * Publish event changing focused pane.
 *
 * @param {string} pane the pane to change focus
 * @return {Rx.Observable}
 */
export function changeCurrentPane(pane: string): Rx.Observable<action.pane.PaneChanger> {
  'use strict';
  return Rx.Observable.just({key: ACTIONS.CHANGE_FOCUSED_PANE, pane});
}
