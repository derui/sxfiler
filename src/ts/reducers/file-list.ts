// reducers for file list
import { Actions } from "../actions";
import { empty, State } from "../states/file-list";
import { ActionTypes } from "../actions/filer";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.initialize:
      return {
        ...state,
        initialized: true,
        left: action.payload.left,
        right: action.payload.right,
      };
  }
  return state;
}

export default reducer;
