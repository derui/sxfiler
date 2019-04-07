import { Notifications, createNotifications } from "../domains/notifications";

export interface State {
  // all notification not finished yet
  notifications: Notifications;
  timeouts: Notifications;
}

// get empty state
export function empty(): State {
  return {
    notifications: createNotifications([]),
    timeouts: createNotifications([]),
  };
}
