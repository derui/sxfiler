// define reducer for UI context
import { Actions, ActionTypes } from "@/actions";
import { UIContext } from "@/types/ui-context";
import { createAppContext, AppContextObject } from "@/domains/app-context";

export type State = AppContextObject;

export function reducer(state = createAppContext({ current: UIContext.OnFileTree }).plain(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.TASK_REQUIRE_INTERACTION:
      return createAppContext(state)
        .changeCurrent(UIContext.OnSuggestion)
        .plain();
    case ActionTypes.TASK_FINISHED:
      return createAppContext(state)
        .changeCurrent(UIContext.OnFileTree)
        .plain();
    case ActionTypes.HISTORY_OPEN:
      return createAppContext(state)
        .changeCurrent(UIContext.OnCompletion)
        .addSubContext(UIContext.ForHistory)
        .plain();
    case ActionTypes.HISTORY_CLOSE:
      return createAppContext(state)
        .changeCurrent(UIContext.OnFileTree)
        .removeSubContext(UIContext.ForHistory)
        .plain();
    default:
      return state;
  }
}
