import Notifications from "../../domains/notifications";

export interface State {
  // all notification not finished yet
  notifications: Notifications;
  timeouted: Notifications;
}

// get empty state
export function empty(): State {
  return {
    notifications: new Notifications(),
    timeouted: new Notifications(),
  };
}
