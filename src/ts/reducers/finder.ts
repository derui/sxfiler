// reducer for history completion
import { Actions, ActionTypes } from "@/actions";
import { State, empty } from "@/states/finder";
import { moveCursor, replaceCandidates } from "@/domains/completion";
import { FileItem } from "@/domains/file-item";
import { createCandidate } from "@/domains/candidate";

const toCompletion = function convertItemToCompletion(item: FileItem) {
  return createCandidate({
    id: item.id,
    value: item.name,
  });
};

export const reducer = function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.FINDER_OPEN:
      return {
        opened: true,
        side: action.side,
        completion: replaceCandidates(action.items.map(toCompletion))(state.completion),
      };
    case ActionTypes.FINDER_CLOSE:
      return { ...state, opened: false };
    case ActionTypes.FINDER_CURSOR_UP:
      return { ...state, completion: moveCursor(-1)(state.completion) };
    case ActionTypes.FINDER_CURSOR_DOWN:
      return { ...state, completion: moveCursor(1)(state.completion) };
    case ActionTypes.FINDER_REPLACE_CANDIDATES:
      return { ...state, completion: replaceCandidates(action.candidates)(state.completion) };
    default:
      return state;
  }
};
