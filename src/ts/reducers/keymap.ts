// reducers for keymap
import { Actions } from "../actions";
import { ActionTypes } from "../actions/keymap";
import { empty, State } from "../states/keymap";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.getKeymap:
      return { current: action.payload.keymap };
  }
  return state;
}

export default reducer;
