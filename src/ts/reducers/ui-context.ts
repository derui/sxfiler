// define reducer for UI context
import { Actions } from "../actions";
import { ActionTypes } from "../actions/ui-context";
import UIContext from "../types/ui-context";

export default function reducer(state = UIContext.OnFileTree, action: Actions) {
  switch (action.type) {
    case ActionTypes.enableFileTree:
      return UIContext.OnFileTree;
    case ActionTypes.enablePreview:
      return UIContext.OnPreview;
    default:
      break;
  }

  return state;
}
