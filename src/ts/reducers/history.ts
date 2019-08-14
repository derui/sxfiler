// reducer for history completion
import { Actions, ActionTypes } from "@/actions";
import { State, empty } from "@/states/history";
import { moveCursor, replaceCandidates } from "@/domains/completion";

export const reducer = function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.HISTORY_OPEN:
      return { ...state, opened: true, side: action.side };
    case ActionTypes.HISTORY_CLOSE:
      return { ...state, opened: false };
    case ActionTypes.HISTORY_CURSOR_UP:
      return { ...state, completion: moveCursor(-1)(state.completion) };
    case ActionTypes.HISTORY_CURSOR_DOWN:
      return { ...state, completion: moveCursor(1)(state.completion) };
    case ActionTypes.HISTORY_REPLACE_CANDIDATES:
      return { ...state, completion: replaceCandidates(action.candidates)(state.completion) };
    default:
      return state;
  }
};
