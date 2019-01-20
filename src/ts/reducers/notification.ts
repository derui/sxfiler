// reducers for notification
import { Actions } from "../actions";
import { ActionTypes } from "../actions/notification";
import { empty, State } from "../types/store-state/notification";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.timeout:
      return { ...state, notifications: state.notifications.remove(action.notificationId) };
    case ActionTypes.notify:
      return { ...state, notifications: state.notifications.append(action.notification) };
  }
  return state;
}

export default reducer;
