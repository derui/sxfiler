import { Keymap, createKeymap } from "@/domains/keymap";

export type State = {
  // current key map
  readonly current: Keymap;
  readonly allKeymap: Keymap;
};

/** find key binding by key */
export function findBinding(state: State, key: string) {
  return state.current.find(key);
}

/** return empty state */
export function empty(): State {
  return { current: createKeymap(), allKeymap: createKeymap() };
}
