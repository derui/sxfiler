// define reducer for UI context
import { Actions } from "../actions";
import UIContext from "../types/ui-context";
import { ActionTypes } from "../actions/task";

export default function reducer(state = UIContext.OnFileTree, action: Actions) {
  switch (action.type) {
    case ActionTypes.requireInteraction:
      return UIContext.OnSuggestion;
    case ActionTypes.finished:
      return UIContext.OnFileTree;
    default:
      break;
  }

  return state;
}
