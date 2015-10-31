
import R from 'ramda';
import {IPCKeys, Pane} from 'sxfiler/common/Constants';
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
      leftPane: R.merge(value.leftPane, data)
    });
    break;
  case Pane.RIGHT:
    return R.merge(value, {
      rightPane: R.merge(value.rightPane, data)
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
    this._ipcDispose = {};
  }

  /**
   * Apply to update value of RECEIVE_DIRECTORY event.
   * 
   * @param {RendererIPC} ipc A wrapper of IPC
   * @param {Store} store a store to update value
   * @param {object} action a action state from an action
   */
  receiveDirectory(ipc, store, action) {
    let {path, fileList, pane} = action;
    let data = {fileList, path};

    store.update((value) => mergeState(value, data, pane));
  }

  /**
   * Apply to update value of REQUEST_DIRECTORY event.
   *
   * @param {RendererIPC} ipc A wrapper of IPC
   * @param {Store} store a store to update value
   * @param {object} action a action state from an action
   */
  requestDirectory(ipc, store, action) {
    let {path, pane} = action;
    let data = {path};

    store.update((value) => mergeState(value, data, pane));
    ipc.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, path, pane);
  }

  /**
   * Execute bindings with Directory action and given store.
   *
   * @param {RendererIPC} ipc IPC wrapper to send and handle events
   * @param {Store} store A Store instance store directory informations
   */
  bind(ipc, store) {
    const A = Actions.ACTIONS;

    this.dispose();

    // Setup binding with observable of directory actions.
    this._dispose = Actions.subject.subscribe((action) => {
      let {key} = action;
      switch (key) {
      case A.RECEIVE_DIRECTORY:
        this.receiveDirectory(ipc, store, action);
        break;
      case A.REQUEST_DIRECTORY:
        this.requestDirectory(ipc, store, action);
        break;
      default:
        break;
      }
    });

    this._bindIpcEvents(ipc);
  }

  /**
   * Binding functions to ipc events.
   * 
   * @private
   * @param {RendererIPC} ipc ipc wrapper to send events and handle it
   */
  _bindIpcEvents(ipc) {
    let key = IPCKeys.FINISH_FILES_IN_DIRECTORY;
    this._ipcDispose[key] =
      ipc.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, path, files, pane]) => {
        if (err) {
          return;
        }

        Actions.receiveDirectory(path, files, pane);
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
    R.pipe(R.keys, R.map((key) => this._ipcDispose[key].dispose()))(this._ipcDispose);
  }
}
