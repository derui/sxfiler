import { Notification } from "../domains/notification";
import { AppAction } from "./type";

export enum ActionTypes {
  timeout = "notification_timeout",
  receiveNotification = "notification_receive_notification",
  remove = "notification_remove",
}

type TimeoutAction = AppAction<ActionTypes.timeout, { notificationId: string }>;
type ReceiveNotificationAction = AppAction<ActionTypes.receiveNotification, { notification: Notification }>;
type RemoveAction = AppAction<ActionTypes.remove, { notificationId: string }>;

export type Actions = TimeoutAction | ReceiveNotificationAction | RemoveAction;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: ActionTypes.timeout, notificationId };
};

const receiveNotification = (notification: Notification): ReceiveNotificationAction => {
  return { type: ActionTypes.receiveNotification, notification };
};

const remove = (notificationId: string): RemoveAction => {
  return { type: ActionTypes.remove, notificationId };
};

export const actions = { timeout, receiveNotification, remove };
