import { AppAction } from "./type";
import Types from "./types/notification";

type TimeoutAction = AppAction<Types.timeout, { notificationId: string }>;

export type Actions = TimeoutAction;
