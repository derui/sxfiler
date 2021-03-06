//#IMPORT INDICATOR
import * as Theme from "./theme";
import * as LogEvent from "./log-event";
import * as Configuration from "./configuration";
import * as Keymap from "./keymap";
import * as Filer from "./filer";
import * as Completer from "./completer";
import * as Decision from "./decision";
// prettier-ignore
import { combineReducers } from "redux";

// export combined types
// prettier-ignore
export type Actions =
  | Theme.Actions
  | LogEvent.Actions
  | Configuration.Actions
  | Keymap.Actions
  | Filer.Actions
  | Completer.Actions
  | Decision.Actions
  ;

// export combined reducer
// prettier-ignore
export const reducer = combineReducers({
  theme: Theme.reducer,
  logEvent: LogEvent.reducer,
  configuration: Configuration.reducer,
  keymap: Keymap.reducer,
  filer: Filer.reducer,
  completer: Completer.reducer,
  decision: Decision.reducer,
})

export const emptyState = {
  theme: Theme.emptyState,
  logEvent: LogEvent.emptyState,
  configuration: Configuration.emptyState,
  keymap: Keymap.emptyState,
  filer: Filer.emptyState,
  completer: Completer.emptyState,
  decision: Decision.emptyState,
};

// export state type
export type State = ReturnType<typeof reducer>;
