import { AppAction, ActionTypes } from "./type";
import { Suggestions } from "@/domains/task-suggestion";
import { Reply, ReplyPayload } from "@/domains/task-reply";

type RequireInteractionAction = AppAction<ActionTypes.TASK_REQUIRE_INTERACTION, { suggestions: Suggestions }>;
type SendReplyAction = AppAction<ActionTypes.TASK_SEND_REPLY, { reply: Reply }>;
type FinishedAction = AppAction<ActionTypes.TASK_FINISHED, { taskId: string }>;
type SelectReplyAction = AppAction<ActionTypes.TASK_SELECT_REPLY, { index: number }>;
type UpdateReplyPayloadAction = AppAction<ActionTypes.TASK_UPDATE_REPLY_PAYLOAD, { payload: ReplyPayload }>;

export type Actions = RequireInteractionAction | FinishedAction | SelectReplyAction | UpdateReplyPayloadAction;

const requireInteraction = (suggestions: Suggestions): RequireInteractionAction => {
  return { type: ActionTypes.TASK_REQUIRE_INTERACTION, suggestions };
};

const finished = (taskId: string): FinishedAction => {
  return { type: ActionTypes.TASK_FINISHED, taskId };
};

const sendReply = (reply: Reply): SendReplyAction => {
  return { type: ActionTypes.TASK_SEND_REPLY, reply };
};

const selectReply = (index: number): SelectReplyAction => {
  return { type: ActionTypes.TASK_SELECT_REPLY, index };
};

const updateReplyPayload = (payload: ReplyPayload): UpdateReplyPayloadAction => {
  return { type: ActionTypes.TASK_UPDATE_REPLY_PAYLOAD, payload };
};

export const actions = { requireInteraction, finished, sendReply, selectReply, updateReplyPayload };
