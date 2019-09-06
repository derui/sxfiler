import { Actions as NotificationActions } from "./notification";
import { Actions as FilerActions } from "./filer";
import { Actions as KeymapActions } from "./key-map";
import { Actions as TaskActions } from "./task";
import { Actions as BookmarkAction } from "./bookmark";
import { Actions as CompleterAction } from "./completer";

export type Actions =
  | NotificationActions
  | FilerActions
  | KeymapActions
  | TaskActions
  | BookmarkAction
  | CompleterAction;

export { ActionTypes } from "./type";
