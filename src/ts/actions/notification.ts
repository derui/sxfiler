import { Notification } from "../domains/notification";
import { AppAction } from "./type";

export enum ActionTypes {
  timeout = "notification_timeout",
  notify = "notification_notify",
  remove = "notification_remove",
}

type TimeoutAction = AppAction<ActionTypes.timeout, { notificationId: string }>;
type NotifyAction = AppAction<ActionTypes.notify, { notification: Notification }>;
type RemoveAction = AppAction<ActionTypes.remove, { notificationId: string }>;

export type Actions = TimeoutAction | NotifyAction | RemoveAction;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: ActionTypes.timeout, notificationId };
};

const notify = (notification: Notification): NotifyAction => {
  return { type: ActionTypes.notify, notification };
};

const remove = (notificationId: string): RemoveAction => {
  return { type: ActionTypes.remove, notificationId };
};

export const actions = { timeout, notify, remove };
