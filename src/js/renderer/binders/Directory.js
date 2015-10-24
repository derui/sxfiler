
import R from 'ramda';
import {Pane} from 'sxfiler/common/Constants';
import * as Actions from 'sxfiler/renderer/actions/Directory';

/**
 * @param {object} value a state of the store
 * @param {object} data a data to update value
 * @param {Pane} pane A pane of target to update
 */
function mergeState(value, data, pane) {
  switch (pane) {
  case Pane.LEFT:
    return R.merge(value, {
      leftPane: R.merge(data.leftPane, data)
    });
    break;
  case Pane.RIGHT:
    return R.merge(value, {
      rightPane: R.merge(data.rightPane, data)
    });
  default:
    return value;
  }
}

/**
 * Binder for Directory actions and Directory store.
 *
 * Warning, you must not call more than once {DIrectory#bind} method.
 */
export default class Directory {
  /**
   * Construct binder to Directory actions
   *
   * Specified store publish initial value when construct binder.
   *
   * @param {Store} store - a store to bind and store directory informations
   */
  constructor() {
    this._dispose = null;
  }

  /**
   * Apply to update value of RECEIVE_DIRECTORY event.
   * 
   * @param {Store} store a store to update value
   * @param {object} action a action state from an action
   */
  receiveDirectory(store, action) {
    let {fileList, pane} = action;
    let data = {fileList};
    let value = store.currentValue();

    store.update(mergeState(value, data, pane));
  }

  /**
   * Apply to update value of REQUEST_DIRECTORY event.
   * 
   * @param {Store} store a store to update value
   * @param {object} action a action state from an action
   */
  requestDirectory(store, action) {
    let {path, pane} = action;
    let data = {path};
    let value = store.currentValue();

    store.update(mergeState(value, data, pane));
  }

  /**
   * Execute bindings with Directory action and given store.
   */
  bind(store) {
    const A = Actions.ACTIONS;

    this.dispose();

    // Setup binding with observable of directory actions.
    this._dispose = Actions.subject.subscribe((action) => {
      let {key} = action;
      switch (key) {
      case A.RECEIVE_DIRECTORY:
        this.receiveDirectory(store, action);
        break;
      case A.REQUEST_DIRECTORY:
        this.requestDirectory(store, action);
        break;
      default:
        break;
      }
    });
  }

  /**
   * Unsubscribe from action subject.
   */
  dispose() {
    if (!this._dispose) {
      return;
    }

    this._dispose.dispose();
  }
}
