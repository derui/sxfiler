// define functional library in TypeScript

// pipe multiple function at once
export const pipe = function pipe<T extends any[], R>(fn1: (...args: T) => R, ...fns: Array<(arg: R) => R>) {
  const piped = fns.reduce(
    (current, next) => (value) => next(current(value)),
    (value) => value
  );

  return (...args: T) => piped(fn1(...args));
};

// compose two function
export const compose = function compose<V, R, N>(fn1: (arg: V) => R, fn2: (arg: R) => N) {
  return (v: V) => fn2(fn1(v));
};

/**
 * port Kotlin's `also` extend function.
 *
 * WARN: This function be able to modify the object in function. Avoid to use this funciton
 *       for shared function.
 */
export const also = function also<T>(it: T, f: (it: T) => void): T {
  f(it);
  return it;
};
