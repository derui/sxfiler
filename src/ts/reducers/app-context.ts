// define reducer for UI context
import { Actions, ActionTypes } from "@/actions";
import { UIContext } from "@/types/ui-context";
import { createAppContext, AppContext, changeCurrent, addSubContext, removeSubContext } from "@/domains/app-context";
import { pipe } from "@/libs/fn";

export type State = AppContext;

export const reducer = function reducer(
  state = createAppContext({ current: UIContext.OnFileTree }),
  action: Actions
): State {
  switch (action.type) {
    case ActionTypes.TASK_REQUIRE_INTERACTION:
      return changeCurrent(UIContext.OnSuggestion)(state);
    case ActionTypes.TASK_FINISHED:
      return changeCurrent(UIContext.OnFileTree)(state);
    case ActionTypes.COMPLETER_OPEN:
      return pipe(
        changeCurrent(UIContext.OnCompletion),
        addSubContext(action.context)
      )(state);
    case ActionTypes.COMPLETER_CLOSE:
      return pipe(
        changeCurrent(UIContext.OnFileTree),
        removeSubContext(action.context)
      )(state);
    default:
      return state;
  }
};
