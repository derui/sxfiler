// reducers for notification
import { Actions } from "../actions";
import ActionTypes from "../actions/types/notification";
import { empty, State } from "../types/store-state/notification";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.timeout:
      return { ...state, notifications: state.notifications.remove(action.notificationId) };
  }
  return state;
}

export default reducer;
