import { Action } from "redux";

export type AppAction<T extends string, Extra extends {} = {}> = Action<T> & { [K in keyof Extra]: Extra[K] };

export enum ActionTypes {
  HISTORY_CURSOR_UP = "HISTORY_CURSOR_UP",
  HISTORY_CURSOR_DOWN = "HISTORY_CURSOR_DOWN",
  HISTORY_REPLACE_CANDIDATES = "HISTORY_REPLACE_CANDIDATES",
  HISTORY_SELECT = "HISTORY_SELECT",
  FILER_UPDATE = "FILER_UPDATE_FILER",
  FILER_LOAD = "FILER_LOAD_FILER",
  FILER_RELOAD = "FILER_RELOAD",
  FILER_CHANGE_SIDE = "FILER_CHANGE_SIDE",
  KEYMAP_UPDATE = "KEYMAP_UPDATE",
  NOTIFICATION_TIMEOUT = "notification_timeout",
  NOTIFICATION_RECEIVE_MESSAGE = "notification_receive_message",
  NOTIFICATION_RECEIVE_PROGRESS = "notification_receive_progress",
  NOTIFICATION_REMOVE = "notification_remove",
  TASK_REQUIRE_INTERACTION = "TASK_REQUIRE_INTERACTION",
  TASK_SEND_REPLY = "TASK_SEND_REPLY",
  TASK_FINISHED = "TASK_FINISHED",
  TASK_SELECT_REPLY = "TASK_SELECT_REPLY",
  TASK_UPDATE_REPLY_PAYLOAD = "TASK_UPDATE_REPLY_PAYLOAD",
}
