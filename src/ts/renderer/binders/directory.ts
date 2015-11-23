
import * as Rx from 'rx';
import R from 'ramda';
import * as Actions from 'sxfiler/renderer/actions/directory';
import {ACTIONS as PaneAction} from 'sxfiler/renderer/actions/pane-operations';
import Store from 'sxfiler/renderer/stores/store';
import {PaneInfo} from 'sxfiler/renderer/types';
import {ActionStream} from 'sxfiler/renderer/action-stream';

/**
 * @param {object} state a current state of a store
 * @param {object} action An action from actionStream
 * @return {object} new or current state
 */
function mergeState(state: PaneInfo, action: any): PaneInfo {
  'use strict';
  switch (action.key) {
  case Actions.ACTIONS.REQUEST_DIRECTORY:
    if (action.pane === state.pane) {
      return R.merge(state, {currentPath: action.path});
    }
    return state;
  case Actions.ACTIONS.RECEIVE_DIRECTORY:
    if (action.pane === state.pane) {
      return R.merge(state, {currentPath: action.path, fileList: action.fileList});
    }
    return state;
  case PaneAction.MOVE_CURSOR: {
    let currentFiles = Math.max(0, state.fileList.length - 1);
    if (action.pane === state.pane) {
      return R.merge(state, {
        selected: Math.min(currentFiles, Math.max(0, state.selected + action.amount))
      });
    }
  }
    return state;
  default:
    return state;
  }
}

/**
 * Binder for Directory actions and Directory store.
 *
 * Warning, you must not call more than once {DIrectory#bind} method.
 */
export default class Directory {
  private _actionStream: ActionStream;
  private _store: Store<PaneInfo>;

  /**
   * Get store of this binder.
   *
   * @return {Store} the store stored state
   */
  get store(): Store<PaneInfo> { return this._store; }

  private _dispose: Rx.Disposable;

  /**
   * Construct binder to Directory actions
   *
   * Specified store publish initial value when construct binder.
   *
   * @param {Rx.observable} actionStream - The action stream that is streaming all action in this 
   */
  constructor(pane: string, actionStream: ActionStream) {
    this._actionStream = actionStream;
    this._store = new Store({
      currentPath: '',
      pane,
      fileList: [],
      selected: 0
    });

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
