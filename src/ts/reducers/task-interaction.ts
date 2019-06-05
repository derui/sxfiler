// Task interaction manages the state for task suggestions and replys
import { Actions } from "../actions";
import { ActionTypes } from "../actions/task";
import * as TaskInteractionState from "../states/task-interaction";

export default function reducer(state = TaskInteractionState.empty(), action: Actions) {
  switch (action.type) {
    case ActionTypes.requireInteraction:
      return TaskInteractionState.giveSuggestions(state, action.suggestions);
    case ActionTypes.finished:
      // TODO
      break;
    case ActionTypes.selectSuggestion:
      return TaskInteractionState.selectSuggestion(state, action.index);
    case ActionTypes.updateReplyPayload:
      return TaskInteractionState.updateReplyPayload(state, action.payload);
  }
  return state;
}
