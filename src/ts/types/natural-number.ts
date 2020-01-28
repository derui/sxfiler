/**
 * This module provides a simple data type that constraints only natural number.
 */

/**
 * wrapped type
 */
export type NaturalNumber = {
  readonly value: number;
};

/**
 * increment natural value
 */
export const inc = function increment(value: NaturalNumber): NaturalNumber {
  return create(value.value + 1);
};

/**
 * decrement natural value
 */
export const dec = function decrement(value: NaturalNumber): NaturalNumber {
  return create(value.value - 1);
};

/**
 * add natural values
 */
export const add = function add(v1: NaturalNumber, v2: NaturalNumber): NaturalNumber {
  return create(v1.value + v2.value);
};

/**
 * subtract natural values
 */
export const sub = function sub(v1: NaturalNumber, v2: NaturalNumber): NaturalNumber {
  return create(v1.value - v2.value);
};

/**
 * multiply natural values
 */
export const mul = function mul(v1: NaturalNumber, v2: NaturalNumber): NaturalNumber {
  return create(v1.value * v2.value);
};

/**
 * divide natural value with other natural value
 */
export const div = function div(v1: NaturalNumber, v2: NaturalNumber): NaturalNumber | undefined {
  if (v2.value === 0) {
    return undefined;
  }

  return create(v1.value / v2.value);
};

/**
 * get maximum number from v1 and v2
 */
export const max = function max(v: NaturalNumber, ...args: NaturalNumber[]): NaturalNumber {
  if (args.length === 0) {
    return v;
  }

  return args.reduce((current, next) => {
    return current.value > next.value ? current : next;
  }, v);
};

/**
 * get minimum number from v1 and v2
 */
export const min = function min(v: NaturalNumber, ...args: NaturalNumber[]): NaturalNumber {
  if (args.length === 0) {
    return v;
  }

  return args.reduce((current, next) => {
    return current.value < next.value ? current : next;
  }, v);
};

/**
 * create a natural number from number.
 */
export const create = function createNaturalNumber(value: number = 0): NaturalNumber {
  let validated = value < 0 ? 0 : value;
  validated = Math.floor(validated);

  return Object.freeze({
    get value() {
      return validated;
    },
  });
};
