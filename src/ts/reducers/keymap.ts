// reducers for keymap
import { Actions } from "../actions";
import { ActionTypes } from "../actions/ui-context";
import { ActionTypes as KeymapActionTypes } from "../actions/key-map";
import { empty, State } from "../states/keymap";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.enableFileTree:
      return { current: action.keymap };
    case ActionTypes.enableSuggestion:
      return { current: action.keymap };
    case KeymapActionTypes.updateKeymap:
      return { current: action.keymap };
  }
  return state;
}

export default reducer;
