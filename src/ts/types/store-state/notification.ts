import { Notifications } from "../../domain/notification";

export interface State {
  // all notification not finished yet
  notifications: Notifications;
}

// get empty state
export function empty(): State {
  return { notifications: new Notifications() };
}