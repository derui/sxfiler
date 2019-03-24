import { Actions as NotificationActions } from "./notification";
import { Actions as UIContextActions } from "./ui-context";
import { Actions as FilerActions } from "./filer";

export type Actions = NotificationActions | UIContextActions | FilerActions;
