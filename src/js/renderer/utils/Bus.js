import Rx from 'rx';

/**
 * Simple key-oriented subject manager.
 *
 * This class provide simple key-oriented subject, and helper method to notify observables
 * subscribing a bus.
 */
export default class Bus {

  /**
   * Construct a Bus
   */
  constructor() {
    this._bus = {};
  }

  /**
   * Push values into a subject.
   *
   * Allocate new subject if not have subject specified key.
   *
   * Warning: observable subscribing a bus in this will give *Array* as pushed values.
   *
   * @param {string} key - name of the bus
   * @param {...any} args - arguments to push bus
   */
  push(key, ...args) {
    let bus = this.bus(key);

    bus.onNext(args);
  }

  /**
   * Get the bus.
   * 
   * @param {string} key - name of the bus
   * @return {Rx.Observable} - the bus
   */
  bus(key) {
    if (!this._bus[key]) {
      this._bus[key] = new Rx.ReplaySubject(1);
    }

    return this._bus[key];
  }
}
