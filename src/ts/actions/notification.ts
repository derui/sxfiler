import { AppAction, ActionTypes } from "./type";
import { ProgressNotification } from "@/domains/progress-notification";
import { MessageNotification } from "@/domains/message-notification";

type TimeoutAction = AppAction<ActionTypes.NOTIFICATION_TIMEOUT, { notificationId: string }>;
type ReceiveMessageAction = AppAction<ActionTypes.NOTIFICATION_RECEIVE_MESSAGE, { notification: MessageNotification }>;
type ReceiveProgressAction = AppAction<
  ActionTypes.NOTIFICATION_RECEIVE_PROGRESS,
  { notification: ProgressNotification }
>;
type RemoveAction = AppAction<ActionTypes.NOTIFICATION_REMOVE, { notificationId: string }>;

export type Actions = TimeoutAction | ReceiveMessageAction | ReceiveProgressAction | RemoveAction;

export const timeout = function timeout(notificationId: string): TimeoutAction {
  return { type: ActionTypes.NOTIFICATION_TIMEOUT, notificationId };
};

export const receiveProgress = function receiveProgress(notification: ProgressNotification): ReceiveProgressAction {
  return { type: ActionTypes.NOTIFICATION_RECEIVE_PROGRESS, notification };
};

export const receiveMessage = function receiveMessage(notification: MessageNotification): ReceiveMessageAction {
  return { type: ActionTypes.NOTIFICATION_RECEIVE_MESSAGE, notification };
};

export const remove = function remove(notificationId: string): RemoveAction {
  return { type: ActionTypes.NOTIFICATION_REMOVE, notificationId };
};
