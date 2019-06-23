// reducers for notification
import { Actions } from "../actions";
import { ActionTypes } from "../actions/notification";
import { empty, State } from "../states/log-entry";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.receiveMessage:
      let v = new Map(state.entries);
      v.set(action.notification.id, action.notification);
      return { ...state, entries: v };
  }
  return state;
}

export default reducer;
