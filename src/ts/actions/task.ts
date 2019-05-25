import { AppAction } from "./type";
import { Suggestions } from "../domains/task-suggestion";
import { Reply } from "../domains/task-reply";

export enum ActionTypes {
  requireInteraction = "task_require_interaction",
  sendReply = "task_send_reply",
  finished = "task_finished",
}

type RequireInteractionAction = AppAction<ActionTypes.requireInteraction, { suggestions: Suggestions }>;
type SendReplyAction = AppAction<ActionTypes.sendReply, { reply: Reply }>;
type FinishedAction = AppAction<ActionTypes.finished, { taskId: string }>;

export type Actions = RequireInteractionAction | FinishedAction;

const requireInteraction = (suggestions: Suggestions): RequireInteractionAction => {
  return { type: ActionTypes.requireInteraction, suggestions };
};

const finished = (taskId: string): FinishedAction => {
  return { type: ActionTypes.finished, taskId };
};

const sendReply = (reply: Reply): SendReplyAction => {
  return { type: ActionTypes.sendReply, reply };
};

export const actions = { requireInteraction, finished, sendReply };
