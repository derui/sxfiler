import { AppAction } from "./type";
import { Interaction } from "../domains/task-interaction";

export enum ActionTypes {
  requireInteraction = "task_require_interaction",
  finished = "task_finished",
}

type RequireInteractionAction = AppAction<ActionTypes.requireInteraction, { interaction: Interaction }>;
type FinishedAction = AppAction<ActionTypes.finished, { taskId: string }>;

export type Actions = RequireInteractionAction | FinishedAction;

const requireInteraction = (interaction: Interaction): RequireInteractionAction => {
  return { type: ActionTypes.requireInteraction, interaction };
};

const finished = (taskId: string): FinishedAction => {
  return { type: ActionTypes.finished, taskId };
};

export const actions = { requireInteraction, finished };
