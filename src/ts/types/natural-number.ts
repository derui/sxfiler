/**
 * This module provides a simple data type that constraints only natural number.
 */

/**
 * wrapped type
 */
export type Type = {
  readonly value: number;
};

/**
 * create a natural number from number.
 */
export const create = (value: number = 0): Type => {
  let validated = value < 0 ? 0 : value;
  validated = Math.floor(validated);

  return Object.freeze({
    get value() {
      return validated;
    },
  });
};

export const zero = create(0);

/**
 * increment natural value
 */
export const inc = (value: Type): Type => {
  return create(value.value + 1);
};

/**
 * decrement natural value
 */
export const dec = (value: Type): Type => {
  return create(value.value - 1);
};

/**
 * add natural values
 */
export const add = (v1: Type, v2: Type): Type => {
  return create(v1.value + v2.value);
};

/**
 * subtract natural values
 */
export const sub = (v1: Type, v2: Type): Type => {
  return create(v1.value - v2.value);
};

/**
 * multiply natural values
 */
export const mul = (v1: Type, v2: Type): Type => {
  return create(v1.value * v2.value);
};

/**
 * divide natural value with other natural value
 */
export const div = (v1: Type, v2: Type): Type | undefined => {
  if (v2.value === 0) {
    return undefined;
  }

  return create(v1.value / v2.value);
};

/**
 * get maximum number from v1 and v2
 */
export const max = (v: Type, ...args: Type[]): Type => {
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
export const min = (v: Type, ...args: Type[]): Type => {
  if (args.length === 0) {
    return v;
  }

  return args.reduce((current, next) => {
    return current.value < next.value ? current : next;
  }, v);
};
