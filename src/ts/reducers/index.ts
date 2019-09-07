// define reducers and export root reducer
import { combineReducers, Reducer, ReducersMapObject } from "redux";

import * as actions from "@/actions";
import { AppState } from "@/states";

import { reducer as configReducer } from "./config";
import { reducer as fileListReducer } from "./file-list";
import { reducer as keymapReducer } from "./keymap";
import { reducer as notificationReducer } from "./notification";
import { reducer as appContextReducer } from "./app-context";
import { reducer as taskInteractionReducer } from "./task-interaction";
import { reducer as logEntryReducer } from "./log-entry";
import { reducer as completerReducer } from "./completer";

const reducerMap: ReducersMapObject<AppState, actions.Actions> = {
  context: appContextReducer,
  config: configReducer,
  fileList: fileListReducer,
  keymap: keymapReducer,
  notification: notificationReducer,
  taskInteraction: taskInteractionReducer,
  logEntry: logEntryReducer,
  completer: completerReducer,
};

export const reducer: Reducer<AppState, actions.Actions> = combineReducers(reducerMap);
