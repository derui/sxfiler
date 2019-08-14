// type of notification

export type Body = {
  readonly process: string;
  readonly current: number;
  readonly targeted: number;
};

export type ProgressNotification = {
  readonly id: string;
  readonly body: Body;
};

/**
 * factory function to create progress notification
 * @param id
 * @param level
 * @param progress
 */
export const createProgress = function createProgress(
  id: string,
  progress: { process: string; current: number; targeted: number }
): ProgressNotification {
  return {
    id,
    body: { ...progress },
  };
};
