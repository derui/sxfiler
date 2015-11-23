// define current focused pane
import * as Rx from 'rx';
import R from 'ramda';
import * as Pane from 'sxfiler/renderer/actions/pane-operations';
import Store from 'sxfiler/renderer/stores/store';
import {ActionStream} from 'sxfiler/renderer/action-stream';

/**
 * @param {string} state a current state of a store
 * @param {object} action An action from actionStream
 * @return {string} new or current state
 */
function mergeState(state: string, action: any): string {
  'use strict';
  switch (action.key) {
  case Pane.ACTIONS.CHANGE_FOCUSED_PANE:
    return action.pane;
  default:
    return state;
  }
}

/**
 * Binder for Directory actions and Directory store.
 *
 * Warning, you must not call more than once {DIrectory#bind} method.
 */
export default class CurrentPane {
  private _actionStream: ActionStream;
  private _store: Store<string>;

  /**
   * Get store of this binder.
   *
   * @return {Store} the store stored state
   */
  get store(): Store<string> { return this._store; }

  private _dispose: Rx.Disposable;

  /**
   * Construct binder for pane operations
   *
   * Specified store publish initial value when construct binder.
   *
   * @param {Rx.observable} actionStream - The action stream that is streaming all action in this 
   */
  constructor(pane: string, actionStream: ActionStream) {
    this._actionStream = actionStream;
    this._store = new Store(pane);

    if (this._actionStream) {
      this._dispose = this._actionStream.subscribe((v: any) => {
        this._store.update(R.curry(mergeState)(R.__, v));
      });
    }
  }

  /**
   * unsubscribe from actionStream if unsubscribe yet.
   */
  public dispose(): void {
    if (this._dispose) {
      this._dispose.dispose();
      this._dispose = null;
    }
  }
}
