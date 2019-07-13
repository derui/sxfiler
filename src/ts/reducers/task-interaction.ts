// Task interaction manages the state for task suggestions and replys
import { Actions, ActionTypes } from "@/actions";
import * as TaskInteractionState from "@/states/task-interaction";

export function reducer(state = TaskInteractionState.empty(), action: Actions) {
  switch (action.type) {
    case ActionTypes.TASK_REQUIRE_INTERACTION:
      return TaskInteractionState.giveSuggestions(state, action.suggestions);
    case ActionTypes.TASK_FINISHED:
      // TODO
      break;
    case ActionTypes.TASK_SELECT_REPLY:
      return TaskInteractionState.selectReply(state, action.index);
    case ActionTypes.TASK_UPDATE_REPLY_PAYLOAD:
      return TaskInteractionState.updateCurrentReply(state, action.payload);
  }
  return state;
}
