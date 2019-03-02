// type of notification
export enum NotificationType {
  Message = "message",
  Progress = "progress",
}

export enum Level {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export interface MessageBody {
  kind: "message";
  message: string;
}

export interface ProgressBody {
  kind: "progress";
  process: string;
  current: number;
  target: number;
}

export type Body = MessageBody | ProgressBody;

/**
 * factory function to create one-shot notification
 * @param id
 * @param level
 * @param message
 */
export function createMessage(id: string, level: Level, message: string) {
  return new Notification(id, level, { kind: NotificationType.Message, message });
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
  constructor(public readonly id: string, public readonly level: Level, private body: Body) {}

  public get bodyKind() {
    return this.body.kind;
  }

  // get body as progress.
  public getProgressBody(): ProgressBody {
    switch (this.body.kind) {
      case "message":
        throw Error("can not convert to progress body");
      case "progress":
        return this.body;
    }
  }

  // get body as message.
  public getMessageBody(): MessageBody {
    switch (this.body.kind) {
      case "message":
        return this.body;
      case "progress":
        throw Error("can not convert to one-shot body");
    }
  }
}
