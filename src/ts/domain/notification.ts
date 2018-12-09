
// type of notification
export enum NotificationType {
  OneShot = 'oneshot',
  Progress = 'progress',
}

export enum Level {
  Info = 'info',
  Warning = 'warning',
  Error = 'error'
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

export type Body = OneShotBody | ProgressBody

// notification from server
export class Notification {
  constructor(
    public readonly id: string,
    public readonly level: Level,
    public readonly body: Body
  ) {}
}
