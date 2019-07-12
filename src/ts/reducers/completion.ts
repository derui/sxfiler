// reducers for notification
import { Actions, ActionTypes } from "../actions";
import { State, empty } from "../states/completion";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    default:
      return state;
  }
}

export default reducer;
