import Bus from 'sxfiler/renderer/utils/Bus';
import {IPCKeys} from 'sxfiler/common/Constants';

/**
 * IPC event management for Renderer process.
 *
 * This class is based on {Rx.Observable} to publish event handling, so it only
 * provide `subscribe` and `send` method.
 */
export default class RendererIPC {

  /**
   * Construct RendererIPCHandler
   *
   * Notice for parameter ipc has to require from provided 'require' from 'Electron'.
   *
   * @param {!IPC} ipc ipc module using this
   */
  constructor(ipc) {
    this._ipc = ipc;
    this._bus = new Bus();

    this._ipc.on(IPCKeys.FINISH_FILES_IN_DIRECTORY, this._onFinishFilesInDirectory.bind(this));
  }

  /**
   * Send a message to channel with args.
   *
   * Notice: When you send message from this, you must take channel that have 'REQUEST_' prefix.
   * 
   * @param {string} channel channel for sending message
   * @param {...any} args message to send channel
   */
  send(channel, ...args) {
    this._ipc.send(channel, args);
  }

  /**
   * Subscribe observer to channel bus.
   *
   * @param {string} channel channel for subscribing observer
   * @param {Rx.Observer} observer observer to subscribe for channel
   * @return {Rx.Disposable} The disposable
   */
  subscribe(channel, observer) {
    return this._bus.bus(channel).subscribe(observer);
  }

  /**
   * @param {object} err error information for requesting file informations
   * @param {array[object]} files fs.Stats objects in requested directory
   * @param {Pane} pane side of sending request
   */
  _onFinishFilesInDirectory(err, files, pane) {
    let bus = this._bus.bus(IPCKeys.FINISH_FILES_IN_DIRECTORY);

    bus.onNext([err, files, pane]);
  }
}
