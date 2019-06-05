import { AppAction } from "./type";
import { Suggestions } from "../domains/task-suggestion";
import { Reply, ReplyPayload } from "../domains/task-reply";

export enum ActionTypes {
  requireInteraction = "task_require_interaction",
  sendReply = "task_send_reply",
  finished = "task_finished",
  selectSuggestion = "task_select_suggestion",
  updateReplyPayload = "task_update_reply_payload",
}

type RequireInteractionAction = AppAction<ActionTypes.requireInteraction, { suggestions: Suggestions }>;
type SendReplyAction = AppAction<ActionTypes.sendReply, { reply: Reply }>;
type FinishedAction = AppAction<ActionTypes.finished, { taskId: string }>;
type SelectSuggestionAction = AppAction<ActionTypes.selectSuggestion, { index: number }>;
type UpdateReplyPayloadAction = AppAction<ActionTypes.updateReplyPayload, { payload: ReplyPayload }>;

export type Actions = RequireInteractionAction | FinishedAction | SelectSuggestionAction | UpdateReplyPayloadAction;

const requireInteraction = (suggestions: Suggestions): RequireInteractionAction => {
  return { type: ActionTypes.requireInteraction, suggestions };
};

const finished = (taskId: string): FinishedAction => {
  return { type: ActionTypes.finished, taskId };
};

const sendReply = (reply: Reply): SendReplyAction => {
  return { type: ActionTypes.sendReply, reply };
};

const selectSuggestion = (index: number): SelectSuggestionAction => {
  return { type: ActionTypes.selectSuggestion, index };
};

const updateReplyPayload = (payload: ReplyPayload): UpdateReplyPayloadAction => {
  return { type: ActionTypes.updateReplyPayload, payload };
};

export const actions = { requireInteraction, finished, sendReply, selectSuggestion, updateReplyPayload };
