// reducers for notification
import { Actions } from "../actions";
import { ActionTypes } from "../actions/notification";
import { empty, State } from "../states/notification";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.remove:
      return {
        ...state,
        progresses: state.progresses.remove(action.notificationId),
        timeouts: state.timeouts.remove(action.notificationId),
      };
    case ActionTypes.timeout:
      const notification = state.progresses.findById(action.notificationId);
      if (notification) {
        return { ...state, timeouts: state.timeouts.append(notification) };
      }
      return state;
    case ActionTypes.receiveProgress:
      return { ...state, progresses: state.progresses.append(action.notification) };
  }
  return state;
}

export default reducer;
