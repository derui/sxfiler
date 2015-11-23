/**
 * An type constructor
 */
export class Data<T> {
  private _hasVal: boolean = false;
  private _val: T = null;

  get hasVal(): boolean {return this._hasVal; }
  get val(): T { return this._val; }

  constructor(hasVal: boolean, val?: T) {
    this._hasVal = hasVal;
    this._val = val;
  }
}

// constant for description not to have value
export const NONE: Data<any> = new Data<any>(false);

/**
 * A map function for Maybe.
 *
 * @param {function(x:X):Y} f - a function to convert value
 * @return {function(mx:Data<X>):Data<Y>} - a function chaining Maybe with converted value
 */
export function fmap<X, Y>(f: (x: X) => Y): (mx: Data<X>) => Data<Y> {
  'use strict';
  return (mx: Data<X>) => (!mx.hasVal) ? <Data<Y>>NONE : new Data<Y>(true, f(mx.val));
}

/**
 * Returning a maybe contained given value
 *
 * @param {X} val - a value wrapping up with maybe
 * @return {Data<X>} - a wrapped up maybe monad
 */
export function just<X>(val: X): Data<X> {
  'use strict';
  return new Data<X>(true, val);
}

/**
 * A multiplier for maybe.
 *
 * @param {Data<Data<X>>} mmx - value to flat maybe
 * @return {Data<X>} - flatten maybe
 */
export function flatten<X>(mmx: Data<Data<X>>): Data<X> {
  'use strict';
  return (!mmx.hasVal) ? <Data<X>>NONE : mmx.val;
}

/**
 * Detect given maybe having value or not
 *
 * @param {Data<X>} mx - value to detect
 * @return {boolean} - is mx have value or not
 */
export function isSome<X>(mx: Data<X>): boolean {
  'use strict';
  return mx.hasVal;
}

/**
 * Detect given maybe not having value or not
 *
 * @param {Data<X>} mx - value to detect
 * @return {boolean} - is mx not have value or not
 */
export function isNone<X>(mx: Data<X>): boolean {
  'use strict';
  return !mx.hasVal;
}
