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

function timeout(notificationId: string): TimeoutAction {
  return { type: ActionTypes.NOTIFICATION_TIMEOUT, notificationId };
}

function receiveProgress(notification: ProgressNotification): ReceiveProgressAction {
  return { type: ActionTypes.NOTIFICATION_RECEIVE_PROGRESS, notification };
}

function receiveMessage(notification: MessageNotification): ReceiveMessageAction {
  return { type: ActionTypes.NOTIFICATION_RECEIVE_MESSAGE, notification };
}

function remove(notificationId: string): RemoveAction {
  return { type: ActionTypes.NOTIFICATION_REMOVE, notificationId };
}

export const actions = { timeout, receiveProgress, receiveMessage, remove };
