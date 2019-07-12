// reducers for notification
import { Actions, ActionTypes } from "../actions";
import { empty, State } from "../states/notification";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.NOTIFICATION_REMOVE:
      return {
        ...state,
        progresses: state.progresses.remove(action.notificationId),
        timeouts: state.timeouts.remove(action.notificationId),
      };
    case ActionTypes.NOTIFICATION_TIMEOUT:
      const notification = state.progresses.findById(action.notificationId);
      if (notification) {
        return { ...state, timeouts: state.timeouts.append(notification) };
      }
      return state;
    case ActionTypes.NOTIFICATION_RECEIVE_PROGRESS:
      return { ...state, progresses: state.progresses.append(action.notification) };
  }
  return state;
}

export default reducer;
