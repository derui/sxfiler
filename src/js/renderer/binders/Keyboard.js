import R from 'ramda';
import {IPCKeys, Pane} from 'sxfiler/common/Constants';
import * as Actions from 'sxfiler/renderer/actions/Keyboard';

/**
 * Binder between Keyboard actions and store of state.
 */
export default class Keyboard {
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
   * @private
   * @param {Store} store A Store instance
   * @param {object} action action emitted from Keyboard Action
   */
  moveCursor(store, action) {
    let {direction, pane} = action;

    store.update((value) => {
      switch (pane) {
      case Pane.LEFT:
        return R.merge(value, {
          left: R.merge(value.left, {
            position: value.left.position + direction
          })
        });
      case Pane.RIGHT:
        return R.merge(value, {
          right: R.merge(value.right, {
            position: value.right.position + direction
          })
        });
      default:
        return value;
      }
    });
  }

  /**
   * @private
   * @param {Store} store A Store instance
   * @param {object} action action emitted from Keyboard action
   */
  changePane(store, action) {
    let {pane} = action;
    store.update((value) => {
      return R.merge(value, {
        current: pane
      });
    });
  }

  /**
   * @param {RendererIPC} ipc A wrapper for IPC
   * @param {Store} store A Store instance
   * @param {object} action action emitted from Keyboard action
   */
  changeDirectory(ipc, store, action) {
    let {path, pane} = action;

    store.update((value) => {
      switch (pane) {
      case Pane.LEFT:
        return R.merge(value, {
          left: R.merge(value.left, {
            position: 0
          })
        });
      case Pane.RIGHT:
        return R.merge(value, {
          right: R.merge(value.right, {
            position: 0
          })
        });
      default:
        return value;
      }
    });
    ipc.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, path, pane);
  }

  /**
   * Execute bindings with Directory action and given store.
   *
   * @param {Store} store A Store instance store directory informations
   */
  bind(ipc, store) {
    const A = Actions.ACTIONS;

    this.dispose();

    // Setup binding with observable of directory actions.
    this._dispose = Actions.subject.subscribe((action) => {
      let {key} = action;
      switch (key) {
      case A.MOVE_CURSOR:
        this.moveCursor(store, action);
        break;
      case A.CHANGE_PANE:
        this.changePane(store, action);
        break;
      case A.CHANGE_DIRECTORY:
        this.changeDirectory(ipc, store, action);
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
