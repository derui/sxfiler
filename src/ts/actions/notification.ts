import { AppAction } from "./type";
import { ProgressNotification } from "../domains/progress-notification";
import { MessageNotification } from "../domains/message-notification";

export enum ActionTypes {
  timeout = "notification_timeout",
  receiveMessage = "notification_receive_message",
  receiveProgress = "notification_receive_progress",
  remove = "notification_remove",
}

type TimeoutAction = AppAction<ActionTypes.timeout, { notificationId: string }>;
type ReceiveMessageAction = AppAction<ActionTypes.receiveMessage, { notification: MessageNotification }>;
type ReceiveProgressAction = AppAction<ActionTypes.receiveProgress, { notification: ProgressNotification }>;
type RemoveAction = AppAction<ActionTypes.remove, { notificationId: string }>;

export type Actions = TimeoutAction | ReceiveMessageAction | ReceiveProgressAction | RemoveAction;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: ActionTypes.timeout, notificationId };
};

const receiveProgress = (notification: ProgressNotification): ReceiveProgressAction => {
  return { type: ActionTypes.receiveProgress, notification };
};

const receiveMessage = (notification: MessageNotification): ReceiveMessageAction => {
  return { type: ActionTypes.receiveMessage, notification };
};

const remove = (notificationId: string): RemoveAction => {
  return { type: ActionTypes.remove, notificationId };
};

export const actions = { timeout, receiveProgress, receiveMessage, remove };
