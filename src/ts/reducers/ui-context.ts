// define reducer for UI context
import { Actions, ActionTypes } from "@/actions";
import { UIContext } from "@/types/ui-context";

export function reducer(state = UIContext.OnFileTree, action: Actions) {
  switch (action.type) {
    case ActionTypes.TASK_REQUIRE_INTERACTION:
      return UIContext.OnSuggestion;
    case ActionTypes.TASK_FINISHED:
      return UIContext.OnFileTree;
    default:
      break;
  }

  return state;
}
