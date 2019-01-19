import { AppAction } from "./type";
import Types from "./types/notification";

type TimeoutAction = AppAction<Types.timeout, { notificationId: string }>;

const timeout = (notificationId: string): TimeoutAction => {
  return { type: Types.timeout, notificationId };
};

export const actions = { timeout };
