export enum Level {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export type LogEntryObject = {
  id: string;
  level: Level;
  body: string;
};

export type LogEntry = LogEntryObject;

/**
 * create new log entry from arguments
 */
export const createLogEntry = ({ id, level, body }: { id: string; level: Level; body: string }): LogEntry => {
  return {
    id,
    level,
    body,
  };
};
