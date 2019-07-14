import { Actions as NotificationActions } from "./notification";
import { Actions as FilerActions } from "./filer";
import { Actions as KeymapActions } from "./key-map";
import { Actions as TaskActions } from "./task";
import { Actions as CompletionActions } from "./completion";

export type Actions = NotificationActions | FilerActions | KeymapActions | TaskActions | CompletionActions;

export { ActionTypes } from "./type";
