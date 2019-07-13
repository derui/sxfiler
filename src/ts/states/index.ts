import * as Config from "./config";
import * as FileList from "./file-list";
import * as Keymap from "./keymap";
import * as Notification from "./notification";
import * as TaskInteraction from "./task-interaction";
import * as LogEntry from "./log-entry";
import { UIContext } from "@/types/ui-context";

export type AppState = {
  context: UIContext;
  config: Config.State;
  fileList: FileList.State;
  keymap: Keymap.State;
  notification: Notification.State;
  taskInteraction: TaskInteraction.State;
  logEntry: LogEntry.State;
};

// get empty state
export const empty = (): AppState => {
  return {
    context: UIContext.OnFileTree,
    config: Config.empty(),
    fileList: FileList.empty(),
    keymap: Keymap.empty(),
    notification: Notification.empty(),
    taskInteraction: TaskInteraction.empty(),
    logEntry: LogEntry.empty(),
  };
};
