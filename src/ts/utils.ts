// Utility functions to avoid boilarplate

// short hand function to unrwap undefined/null when considered value does not it.
export const unwrap = function unwrap<T>(v: T | undefined | null): T {
  if (v) {
    return v;
  }

  throw new Error("Can not unwrap");
};

// get generator to get indices across range between start and end.
export const range = function* range(start: number, end: number) {
  for (let i = start; i < end; i++) {
    yield i;
  }
};

// get integer between minvalue and maxvalue. The default value of minvalue is 0.
export const between = function between(value: number, max: number, min = 0): number {
  return Math.max(min, Math.min(value, max));
};
