// reducers for notification
import { Actions, ActionTypes } from "@/actions";
import { empty, State } from "@/states/notification";
import { remove, append } from "@/domains/progress-notifications";

export const reducer = function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.NOTIFICATION_REMOVE:
      return {
        ...state,
        progresses: remove(action.notificationId)(state.progresses),
        timeouts: remove(action.notificationId)(state.timeouts),
      };
    case ActionTypes.NOTIFICATION_TIMEOUT:
      const notification = state.progresses.values[action.notificationId];
      if (notification) {
        return { ...state, timeouts: append(notification)(state.timeouts) };
      }
      return state;
    case ActionTypes.NOTIFICATION_RECEIVE_PROGRESS:
      return { ...state, progresses: append(action.notification)(state.progresses) };
  }
  return state;
};
