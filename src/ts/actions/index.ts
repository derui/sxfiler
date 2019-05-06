import { Actions as NotificationActions } from "./notification";
import { Actions as UIContextActions } from "./ui-context";
import { Actions as FilerActions } from "./filer";
import { Actions as KeymapActions } from "./key-map";
import { Actions as TaskActions } from "./task";

export type Actions = NotificationActions | UIContextActions | FilerActions | KeymapActions | TaskActions;
