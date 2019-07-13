// reducers for keymap
import { Actions, ActionTypes } from "@/actions";
import { empty, State } from "@/states/keymap";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.KEYMAP_UPDATE:
      return { current: action.keymap, allKeymap: action.keymap };
  }
  return state;
}
