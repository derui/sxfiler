// reducers for keymap
import { Actions } from "../actions";
import { ActionTypes } from "../actions/ui-context";
import { empty, State } from "../states/keymap";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.enableFileTree:
      return { current: action.keymap };
    case ActionTypes.enablePreview:
      return { current: action.keymap };
  }
  return state;
}

export default reducer;
