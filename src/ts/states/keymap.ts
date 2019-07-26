import { Keymap, createKeymap, allowedInContext, find } from "@/domains/keymap";
import { AppContext } from "@/domains/app-context";
import { compose } from "@/libs/fn";

export type State = {
  // current key map
  readonly current: Keymap;
  readonly allKeymap: Keymap;
};

/** find key binding by key */
export function findBinding(state: State, context: AppContext, key: string) {
  return compose(
    allowedInContext(context),
    find(key)
  )(state.current);
}

/** return empty state */
export function empty(): State {
  return { current: createKeymap(), allKeymap: createKeymap() };
}
