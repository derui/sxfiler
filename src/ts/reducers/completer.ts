import * as S from "@/states/completer";
import * as C from "@/domains/completion";
import { Actions, ActionTypes } from "@/actions";

export const reducer = function reducer(state: S.State = S.empty(), action: Actions): S.State {
  switch (action.type) {
    case ActionTypes.COMPLETER_OPEN:
      return S.open(action.title)(state);
    case ActionTypes.COMPLETER_CLOSE:
      return S.close(state);

    case ActionTypes.COMPLETER_CURSOR_UP:
      return S.updateCompletion(C.moveCursor(-1)(state.completion))(state);
    case ActionTypes.COMPLETER_CURSOR_DOWN:
      return S.updateCompletion(C.moveCursor(1)(state.completion))(state);
    case ActionTypes.COMPLETER_REPLACE_CANDIDATES:
      return S.updateCompletion(C.replaceCandidates(action.candidates)(state.completion))(state);
    default:
      return state;
  }
};
