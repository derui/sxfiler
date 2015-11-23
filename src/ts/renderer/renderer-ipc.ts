import R from 'ramda';
import Bus from 'sxfiler/renderer/utils/bus';
import {IPC_KEYS} from 'sxfiler/common/constants';
import uuid from 'uuid';

interface Event {
  sender: {
    send: (event: string, ...args: any[]) => void;
  };
}

export interface RendererIPC {
  /**
   * Send a message to channel with args.
   *
   * Notice: When you send message from this, you must take channel that have 'REQUEST_' prefix.
   *
   * Return an observeable sequence that only subscribe event once. You should subscribe it or
   * map/connect/concat... any observeable operations with any other sequence what you want.
   *
   * @param {string} channel channel for sending message
   * @param {...any} args message to send channel
   * @return {Rx.Observerable} An observeable sequence only subscribe once.
   */
  send(channel: string, ...args: any[]): Rx.Observable<any>;

  /**
   * Subscribe observer to channel bus.
   *
   * Using this method if you subscribe all values streaming in specified channel.
   * You do not forget to call dispose from returning disposable.
   *
   * @param {string} channel channel for subscribing observer
   * @param {Rx.Observer} observer observer to subscribe for channel
   * @return {Rx.Disposable} The disposable
   */
  subscribe(channel: string, observer: (val: any) => void): Rx.Disposable;
}

/**
 * IPC event management for Renderer process.
 *
 * This class is based on {Rx.Observable} to publish event handling, so it only
 * provide `subscribe` and `send` method.
 */
export default class RendererIPCImpl implements RendererIPC {

  private _ipc: any;
  private _bus: Bus = new Bus();

  /**
   * Construct RendererIPCHandler
   *
   * Notice for parameter ipc has to require from provided 'require' from 'Electron'.
   *
   * @param {!IPC} ipc ipc module using this
   */
  constructor(ipc: any) {
    this._ipc = ipc;

    this._ipc.on(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, this._onFinishFilesInDirectory.bind(this));
  }

  /**
   * Send a message to channel with args.
   *
   * Notice: When you send message from this, you must take channel that have 'REQUEST_' prefix.
   *
   * Return an observeable sequence that only subscribe event once. You should subscribe it or
   * map/connect/concat... any observeable operations with any other sequence what you want.
   *
   * @param {string} channel channel for sending message
   * @param {...any} args message to send channel
   * @return {Rx.Observerable} An observeable sequence only subscribe once.
   */
  public send(channel: string, ...args: any[]): Rx.Observable<any> {
    let currentId: string = uuid.v4();
    this._ipc.send(channel, ...args.concat([currentId]));

    return this._bus.bus(channel).filter((_args: any[]) => R.last(_args) === currentId).first();
  }

  /**
   * Subscribe observer to channel bus.
   *
   * Using this method if you subscribe all values streaming in specified channel.
   * You do not forget to call dispose from returning disposable.
   *
   * @param {string} channel channel for subscribing observer
   * @param {Rx.Observer} observer observer to subscribe for channel
   * @return {Rx.Disposable} The disposable
   */
  public subscribe(channel: string, observer: (val: any) => void): Rx.Disposable {
    return this._bus.bus(channel).subscribe(observer);
  }

  /**
   * @param {event} ev Event object from ipc event emitter
   * @param {object} err error information for requesting file informations
   * @param {string} path An absolute path what traversed
   * @param {array[object]} files fs.Stats objects in requested directory
   * @param {Pane} pane side of sending request
   * @param {string} uuid The unique event id
   */
  private _onFinishFilesInDirectory(ev: Event, ...args: any[]): void {
    let bus: Rx.Subject<any> = this._bus.bus(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY);

    bus.onNext(args);
  }
}
