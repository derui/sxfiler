// reducers for notification
import { Actions } from "../actions";
import { ActionTypes } from "../actions/notification";
import { empty, State, pushEntry } from "../states/log-entry";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.receiveMessage:
      return pushEntry(state, action.notification);
  }
  return state;
}

export default reducer;
