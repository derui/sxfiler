// type of notification
export enum NotificationType {
  OneShot = "oneshot",
  Progress = "progress",
}

export enum Level {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export interface OneShotBody {
  kind: "oneshot";
  message: string;
}

export interface ProgressBody {
  kind: "progress";
  process: string;
  current: number;
  target: number;
}

export type Body = OneShotBody | ProgressBody;

/**
 * factory function to create one-shot notification
 * @param id
 * @param level
 * @param message
 */
export function createOneShot(id: string, level: Level, message: string) {
  return new Notification(id, level, { kind: NotificationType.OneShot, message });
}

/**
 * factory function to create progress notification
 * @param id
 * @param level
 * @param progress
 */
export function createProgress(
  id: string,
  level: Level,
  progress: { process: string; current: number; target: number }
) {
  return new Notification(id, level, { kind: NotificationType.Progress, ...progress });
}

// notification from server
export class Notification {
  constructor(public readonly id: string, public readonly level: Level, public readonly body: Body) {}
}
