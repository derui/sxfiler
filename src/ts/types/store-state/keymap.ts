import { Keymap } from "../../domains/keymap";

export interface State {
  // current key map
  current: Keymap;
}

/** return empty state */
export function empty(): State {
  return { current: new Keymap() };
}
