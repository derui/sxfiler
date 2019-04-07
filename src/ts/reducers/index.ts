// define reducers and export root reducer
import { combineReducers, Reducer, ReducersMapObject } from "redux";

import * as actions from "../actions";
import { AppState } from "../states";

import configReducer from "./config";
import fileListReducer from "./file-list";
import keymapReducer from "./keymap";
import notificationReducer from "./notification";
import uiContextReducer from "./ui-context";

const reducerMap: ReducersMapObject<AppState, actions.Actions> = {
  context: uiContextReducer,
  config: configReducer,
  fileList: fileListReducer,
  keymap: keymapReducer,
  notification: notificationReducer,
};
const reducer: Reducer<AppState, actions.Actions> = combineReducers(reducerMap);

export default reducer;
