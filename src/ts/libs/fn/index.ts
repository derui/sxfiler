// define functional library in TypeScript

// pipe multiple function at once
export const pipe = function pipe<T extends any[], R>(fn1: (...args: T) => R, ...fns: Array<(arg: R) => R>) {
  const piped = fns.reduce(
    (prev, current) => value => prev(current(value)),
    value => value
  );

  return (...args: T) => piped(fn1(...args));
};

// compose two function
export const compose = function compose<V, R, N>(fn1: (arg: V) => R, fn2: (arg: R) => N) {
  return (v: V) => fn2(fn1(v));
};
