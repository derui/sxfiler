// reducer for history completion
import { Actions, ActionTypes } from "@/actions";
import { State, empty } from "@/states/history";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.HISTORY_OPEN:
      return { ...state, opened: true, side: action.side };
    case ActionTypes.HISTORY_CLOSE:
      return { ...state, opened: false };
    case ActionTypes.HISTORY_CURSOR_UP:
      return { ...state, completion: state.completion.moveCursor(-1) };
    case ActionTypes.HISTORY_CURSOR_DOWN:
      return { ...state, completion: state.completion.moveCursor(1) };
    case ActionTypes.HISTORY_REPLACE_CANDIDATES:
      return { ...state, completion: state.completion.replaceCandidates(action.candidates) };
    default:
      return state;
  }
}
