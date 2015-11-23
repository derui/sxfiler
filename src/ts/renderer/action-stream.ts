import * as Rx from 'rx';
import {EventEmitter} from 'events';
import {RendererIPC} from './renderer-ipc';

type actionArgument = (ipc: RendererIPC) => Rx.Observable<any> | Rx.Observable<any>;

/**
 * An interface of ActionStream
 */
export interface ActionStream {
  /**
   * Publish action to action stream.
   *
   * @param {function(ipc:RendererIPC):Rx.Observable|Rx.Observable} act - an action to publish
   */
  doAction(act: actionArgument): void;

  /**
   * Subscribe events from this.
   *
   * @param {function(value: any)|Rx.Observer<any>} onNext - an onNext callback or observer
   * @param {function(exception: any)} onError - an onError callback
   * @param {function()} onCompleted - an onCompleted callback
   * @return {Rx.IDisposable} a disposable
   */
  subscribe(onNext?: ((value: any) => void|Rx.Observer<any>), onError?: (exception: any) => void, onCompleted?: () => void)
  : Rx.IDisposable;

  /**
   * Dispose published stream from this.
   */
  dispose(): void;
}

/**
 * ActionStream provide action emitter and method to subscribe it.
 */
export default class ActionStreamImpl implements ActionStream {
  private _e: NodeJS.EventEmitter;
  private _broadcastingObservable: Rx.Observable<any>;
  private _broadcastingDisposable: Rx.IDisposable;

  public constructor(ipc: RendererIPC) {
    this._e = new EventEmitter();

    // define observable from EventEmitter
    this._broadcastingObservable = Rx.Observable.fromEventPattern(
      (h: (data: any) => any) => this._e.addListener('action', h),
      (h: (data: any) => any) => this._e.removeListener('action', h),
      (act: any) => {
        if (typeof act === 'function') {
          return (<actionArgument>act)(ipc);
        } else {
          return act;
        }
      }
    ).flatMap((x: Rx.Observable<any>) => x).publish();

    this._broadcastingDisposable = this._broadcastingObservable.connect();
  }

  /**
   * Publish action to action stream.
   *
   * @param {function(ipc:RendererIPC):Rx.Observable|Rx.Observable} act - an action to publish
   */
  public doAction(act: actionArgument): void {
    this._e.emit('action', act);
  }

  /**
   * Subscribe events from this.
   *
   * @param {function(value: any)|Rx.Observer<any>} onNext - an onNext callback or observer
   * @param {function(exception: any)} onError - an onError callback
   * @param {function()} onCompleted - an onCompleted callback
   * @return {Rx.IDisposable} a disposable
   */
  public subscribe(onNext?: ((value: any) => void|Rx.Observer<any>), onError?: (exception: any) => void, onCompleted?: () => void)
  : Rx.IDisposable {
    return this._broadcastingObservable.subscribe(onNext, onError, onCompleted);
  }

  /**
   * Dispose published stream from this.
   */
  public dispose(): void {
    if (this._broadcastingDisposable) {
      this._broadcastingDisposable.dispose();
    }
    this._broadcastingDisposable = null;
  }
}
