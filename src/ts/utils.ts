// Utility functions to avoid boilarplate

// short hand function to unrwap undefined/null when considered value does not it.
export function unwrap<T>(v: T | undefined | null): T {
  if (v) {
    return v;
  }

  throw new Error("Can not unwrap");
}
