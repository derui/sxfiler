import * as Config from "./config";
import * as FileList from "./file-list";
import * as Keymap from "./keymap";
import * as Notification from "./notification";
import * as TaskInteraction from "./task-interaction";
import * as LogEntry from "./log-entry";
import * as H from "./history";
import * as F from "./finder";
import * as Completer from "./completer";
import { AppContext, createAppContext } from "@/domains/app-context";
import { UIContext } from "@/types/ui-context";

export type AppState = {
  context: AppContext;
  config: Config.State;
  fileList: FileList.State;
  keymap: Keymap.State;
  notification: Notification.State;
  taskInteraction: TaskInteraction.State;
  logEntry: LogEntry.State;
  history: H.State;
  finder: F.State;
  completer: Completer.State;
};

// get empty state
export const empty = function emptyAppState(): AppState {
  return {
    context: createAppContext({ current: UIContext.OnFileTree }),
    config: Config.empty(),
    fileList: FileList.empty(),
    keymap: Keymap.empty(),
    notification: Notification.empty(),
    taskInteraction: TaskInteraction.empty(),
    logEntry: LogEntry.empty(),
    history: H.empty(),
    finder: F.empty(),
    completer: Completer.empty(),
  };
};
