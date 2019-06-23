import { createNotifications, ProgressNotifications } from "../domains/progress-notifications";

export interface State {
  // all notification not finished yet
  progresses: ProgressNotifications;
  timeouts: ProgressNotifications;
}

// get empty state
export function empty(): State {
  return {
    progresses: createNotifications([]),
    timeouts: createNotifications([]),
  };
}
