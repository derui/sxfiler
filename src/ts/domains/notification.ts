// type of notification
export enum NotificationKind {
  Message = "message",
  Progress = "progress",
}

export enum Level {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export type MessageBody = {
  readonly message: string;
};

export type ProgressBody = {
  readonly process: string;
  readonly current: number;
  readonly target: number;
};

export type Body = MessageBody | ProgressBody;

export type MessageNotification = {
  readonly id: string;
  readonly kind: "message";
  readonly level: Level;
  readonly body: MessageBody;
};

export type ProgressNotification = {
  readonly id: string;
  readonly kind: "progress";
  readonly level: Level;
  readonly body: ProgressBody;
};

export type Notification = MessageNotification | ProgressNotification;

/**
 * factory function to create one-shot notification
 * @param id
 * @param level
 * @param message
 */
export const createMessage = (id: string, level: Level, message: string): MessageNotification => {
  return { kind: NotificationKind.Message, id, level, body: { message } };
};

/**
 * factory function to create progress notification
 * @param id
 * @param level
 * @param progress
 */
export const createProgress = (
  id: string,
  level: Level,
  progress: { process: string; current: number; target: number }
): Notification => {
  return {
    kind: NotificationKind.Progress,
    id,
    level,
    body: { ...progress },
  };
};
