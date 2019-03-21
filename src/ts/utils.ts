// Utility functions to avoid boilarplate

// short hand function to unrwap undefined/null when considered value does not it.
export function unwrap<T>(v: T | undefined | null): T {
  if (v) {
    return v;
  }

  throw new Error("Can not unwrap");
}

// get generator to get indices across range between start and end.
export function* range(start: number, end: number) {
  for (let i = start; i < end; i++) {
    yield i;
  }
}
