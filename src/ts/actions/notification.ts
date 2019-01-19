import { AppAction } from "./type";
import Types from "./types/notification";
import {Notification} from "../domain/notification";

type TimeoutAction = AppAction<Types.timeout, { notificationId: string }>;
type NotifyAction = AppAction<Types.notify, { notification: Notification }>;

export type Actions = TimeoutAction | NotifyAction;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: Types.timeout, notificationId };
};

export const actions = { timeout };
