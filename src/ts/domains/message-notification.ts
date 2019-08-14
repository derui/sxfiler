export enum Level {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export type MessageNotification = {
  readonly id: string;
  readonly level: Level;
  readonly body: string;
};

/**
 * factory function to create one-shot notification
 * @param id ID of a notification
 * @param level level of a notification
 * @param message body of a message notification
 */
export const createMessage = function createMessage({
  id,
  level,
  message,
}: {
  id: string;
  level: Level;
  message: string;
}): MessageNotification {
  return { id, level, body: message };
};
