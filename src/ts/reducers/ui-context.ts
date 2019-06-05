// define reducer for UI context
import { Actions } from "../actions";
import { ActionTypes } from "../actions/ui-context";
import UIContext from "../types/ui-context";

export default function reducer(state = UIContext.OnFileTree, action: Actions) {
  switch (action.type) {
    case ActionTypes.enableFileTree:
      return UIContext.OnFileTree;
    case ActionTypes.enableSuggestion:
      return UIContext.OnSuggestion;
    default:
      break;
  }

  return state;
}
