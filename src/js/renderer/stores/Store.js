import Rx from 'rx';
import Bus from 'sxfiler/renderer/utils/Bus';

/**
 * Providing simple operations to store any data.
 *
 */
export default class Store {

  /**
   * Construct a store.
   *
   * @param {any} value - Initial value to it
   */
  constructor(value) {
    this._bus = new Bus();
    this._value = value;

    this._subject = new Rx.ReplaySubject(1);
    this._bus.bus('update').subscribe(([v]) => {
      this._value = v;
      this._subject.onNext(v);
    });
    this.update(value);
  }

  /**
   * Subscribes an observer to this store.
   * 
   * @param {Rx.Observer|...(function(v:any))} observer observer to subscribe this store
   * @return {Rx.Disposable} The source sequence whose subscriptions and unsubscriptions happen on the specified scheduler
   */
  subscribe(...observer) {
    return this._subject.subscribe(...observer);
  }

  /**
   * Only update and publish v.
   *
   * @param {any} v a value to publish
   */
  update(v) {
    this._bus.push('update', v);
  }

  /**
   * Get last value called update with.
   * 
   * @return {any} a last value called update with
   */
  currentValue() {
    return this._value;
  }
}
