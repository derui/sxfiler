// Utility functions to avoid boilarplate

// get generator to get indices across range between start and end.
export const range = function* range(start: number, end: number) {
  for (let i = start; i < end; i++) {
    yield i;
  }
};

// get integer between minvalue and maxvalue. The default value of minvalue is 0.
export const between = (value: number, max: number, min = 0): number => Math.max(min, Math.min(value, max));
