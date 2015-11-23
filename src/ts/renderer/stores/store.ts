import * as Rx from 'rx';
import R from 'ramda';
import Bus from 'sxfiler/renderer/utils/bus';
import uuid from 'uuid';

interface Operation<T> {
  oldValue: T;
  transform: (val: T) => T;
  uid: string;
  committed: boolean;
}

interface OperationMap<T> {
  [index: string]: Operation<T>;
}

/**
 * Providing simple operations to store any data.
 *
 */
export default class Store<T> {

  private _bus: Bus = new Bus();
  private _value: T;
  private _operationsStack: string[];
  private _operationsMap: OperationMap<T>;

  /**
   * Get the subject for publishing state from this.
   *
   * User can use subject returned from this with Rx.Observable.combineLatest.
   * 
   * @return {Rx.Subject} subject for publishing state from store
   */
  public getObservable(): Rx.Observable<T> {
    return this._bus.bus('update');
  }

  /**
   * Construct a store.
   *
   * @param {any} value - Initial value to it
   */
  constructor(value: T) {
    this._bus = new Bus();
    this._value = value;
    this._operationsStack = [];
    this._operationsMap = {};
    this._bus.bus('update').onNext(this._value);
  }

  /**
   * Apply operation and update value with taken operation.
   *
   * This method does cleanup store history already commited.
   *
   * @param {function(currentValue:any):any} transform a operation that transform value
   */
  public update(transform: (value: T) => T): void {
    let uid: string = uuid.v4();
    let instance: Operation<T> = {
      committed: false,
      oldValue: this._value,
      transform,
      uid
    };

    this._value = transform(instance.oldValue);
    this._bus.bus('update').onNext(this._value);

    this._operationsMap[uid] = instance;
    this._operationsStack.push(uid);

    this._commitOperation(uid);
  }

  /**
   * Get last value called update with.
   * 
   * @return {any} a last value called update with
   */
  public currentValue(): T {
    return this._value;
  }

  /**
   * Cancel operation specified uuid given.
   * 
   * @private
   * @param {string} uid - UUID of operation to cancel
   */
  private _cancelOperation(uid: string): void {
    if (!this._operationsMap[uid]) {
      return;
    }

    let instance = this._operationsMap[uid];
    let index = this._operationsStack.indexOf(uid);
    // each operation descriptor has old value that the store state at that time.
    // canceling operation should update old value that operation descriptors having.
    let updateAfterOperations = R.pipe(R.drop(index + 1), R.reduce((memo: T, uid: string) => {
      let descriptor = this._operationsMap[uid];
      descriptor.oldValue = memo;
      return descriptor.transform(memo);
    },
                                                                   instance.oldValue));

    this._value = updateAfterOperations(this._operationsStack);

    delete this._operationsMap[uid];
    this._operationsStack = R.reject((id: string) => id === uid, this._operationsStack);
    this._bus.bus('update').onNext(this._value);
  }

  /**
   * Commit operations and delete operations history committed
   * 
   * @private
   * @param {string} uid - UUID of operation to commit
   */
  private _commitOperation(uid: string): void {
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

}
