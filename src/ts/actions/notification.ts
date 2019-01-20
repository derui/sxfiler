import { Notification } from "../domains/notification";
import { AppAction } from "./type";

export enum ActionTypes {
  timeout = "notification_timeout",
  notify = "notification_notify",
}

type TimeoutAction = AppAction<ActionTypes.timeout, { notificationId: string }>;
type NotifyAction = AppAction<ActionTypes.notify, { notification: Notification }>;

export type Actions = TimeoutAction | NotifyAction;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: ActionTypes.timeout, notificationId };
};

const notify = (notification: Notification): NotifyAction => {
  return { type: ActionTypes.notify, notification };
};

export const actions = { timeout, notify };
