// reducers for notification
import { Actions, ActionTypes } from "@/actions";
import * as Completion from "@/states/completion";

export function reducer(state: Completion.State = Completion.empty(), action: Actions): Completion.State {
  switch (action.type) {
    case ActionTypes.COMPLETION_REPLACE_CANDIDATES:
      return Completion.updateCandidates(state, action.candidates);
    case ActionTypes.COMPLETION_CURSOR_UP:
      return Completion.moveCursor(state, -1);
    case ActionTypes.COMPLETION_CURSOR_DOWN:
      return Completion.moveCursor(state, 1);
    default:
      return state;
  }
}
