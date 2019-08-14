// reducers for notification
import { Actions, ActionTypes } from "@/actions";
import { empty, State, pushEntry } from "@/states/log-entry";

export const reducer = function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.NOTIFICATION_RECEIVE_MESSAGE:
      return pushEntry(state, action.notification);
  }
  return state;
};
