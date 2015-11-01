import Rx from 'rx';
import R from 'ramda';
import Bus from 'sxfiler/renderer/utils/Bus';
import uuid from 'uuid';

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
    this._operationsStack = [];
    this._operationsMap = {};
    this.getSubject().onNext(this._value);
  }

  /**
   * Apply operation and update value with taken operation.
   *
   * This method does cleanup store history already commited.
   *
   * @param {function(currentValue:any):any} transform a operation that transform value
   */
  update(transform) {
    let uid = uuid.v4();
    let instance = {
      oldValue: this._value,
      transform,
      uid,
      committed: false
    };

    this._value = transform(instance.oldValue);
    this.getSubject().onNext(this._value);

    this._operationsMap[uid] = instance;
    this._operationsStack.push(uid);

    this._commitOperation(uid);
  }

  /**
   * Cancel operation specified uuid given.
   * 
   * @private
   * @param {string} uid - UUID of operation to cancel
   */
  _cancelOperation(uid) {
    if (!this._operationsMap[uid]) {
      return;
    }
    let instance = this._operationsMap[uid];
    let index = this._operationsStack.indexOf(uid);
    // Each operation descriptor has old value that the store state at that time.
    // Canceling operation should update old value that operation descriptors having.
    let updateAfterOperations = R.pipe(R.drop(index + 1), R.reduce((memo, uid) => {
      let descriptor = this._operationsMap[uid];
      descriptor.oldValue = memo;
      return descriptor.transform(memo);
    }, instance.oldValue));

    this._value = updateAfterOperations(this._operationsStack);

    delete this._operationsMap[uid];
    this._operationsStack = R.reject((id) => id === uid, this._operationsStack);
    this.getSubject().onNext(this._value);
  }

  /**
   * Commit operations and delete operations history committed
   * 
   * @private
   * @param {string} uid - UUID of operation to commit
   */
  _commitOperation(uid) {
    if (!this._operationsMap[uid]) {
      return;
    }
    let instance = this._operationsMap[uid];
    instance.committed = true;
    let lastIndex = -1;

    this._operationsStack.every((uid, index) => {
      if (this._operationsMap[uid].committed) {
        delete this._operationsMap[uid];
        lastIndex = index;
        return true;
      }
      return false;
    });

    this._operationsStack = R.drop(lastIndex + 1, this._operationsStack);
  }

  /**
   * Get the subject for publishing state from this.
   *
   * User can use subject returned from this with Rx.Observable.combineLatest.
   * 
   * @return {Rx.Subject} subject for publishing state from store
   */
  getSubject() {
    return this._bus.bus('update');
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
