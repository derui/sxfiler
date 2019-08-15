import { Actions as NotificationActions } from "./notification";
import { Actions as FilerActions } from "./filer";
import { Actions as KeymapActions } from "./key-map";
import { Actions as TaskActions } from "./task";
import { Actions as HistoryActions } from "./history";
import { Actions as FinderAction } from "./finder";

export type Actions = NotificationActions | FilerActions | KeymapActions | TaskActions | HistoryActions | FinderAction;

export { ActionTypes } from "./type";
