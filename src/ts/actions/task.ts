import { AppAction, ActionTypes } from "./type";
import { Suggestions } from "@/domains/task-suggestion";
import { Reply, ReplyPayload } from "@/domains/task-reply";

type RequireInteractionAction = AppAction<ActionTypes.TASK_REQUIRE_INTERACTION, { suggestions: Suggestions }>;
type SendReplyAction = AppAction<ActionTypes.TASK_SEND_REPLY, { reply: Reply }>;
type FinishedAction = AppAction<ActionTypes.TASK_FINISHED, { taskId: string }>;
type CanceledAction = AppAction<ActionTypes.TASK_CANCELED, { taskId: string }>;
type SelectReplyAction = AppAction<ActionTypes.TASK_SELECT_REPLY, { index: number }>;
type UpdateReplyPayloadAction = AppAction<ActionTypes.TASK_UPDATE_REPLY_PAYLOAD, { payload: ReplyPayload }>;

export type Actions =
  | RequireInteractionAction
  | FinishedAction
  | CanceledAction
  | SelectReplyAction
  | UpdateReplyPayloadAction
  | SendReplyAction;

export const requireInteraction = function requireInteraction(suggestions: Suggestions): RequireInteractionAction {
  return { type: ActionTypes.TASK_REQUIRE_INTERACTION, suggestions };
};

export const finished = function finished(taskId: string): FinishedAction {
  return { type: ActionTypes.TASK_FINISHED, taskId };
};

export const canceled = function canceled(taskId: string): CanceledAction {
  return { type: ActionTypes.TASK_CANCELED, taskId };
};

export const sendReply = function sendReply(reply: Reply): SendReplyAction {
  return { type: ActionTypes.TASK_SEND_REPLY, reply };
};

export const selectReply = function selectReply(index: number): SelectReplyAction {
  return { type: ActionTypes.TASK_SELECT_REPLY, index };
};

export const updateReplyPayload = function updateReplyPayload(payload: ReplyPayload): UpdateReplyPayloadAction {
  return { type: ActionTypes.TASK_UPDATE_REPLY_PAYLOAD, payload };
};
