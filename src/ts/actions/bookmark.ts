import { AppAction, ActionTypes } from "./type";
import { Bookmark } from "@/domains/bookmark";

type RegisterAction = AppAction<ActionTypes.BOOKMARK_REGISTER, { path: string }>;
type DeleteAction = AppAction<ActionTypes.BOOKMARK_DELETE, { id: string }>;
type UpdateAction = AppAction<ActionTypes.BOOKMARK_UPDATE, { bookmarks: Bookmark[] }>;

export type Actions = UpdateAction | DeleteAction | RegisterAction;

/**
   update the key map
 */
export const updateBookmarks = function updateBookmarks(bookmarks: Bookmark[]): UpdateAction {
  return { type: ActionTypes.BOOKMARK_UPDATE, bookmarks };
};

export const registerBookmark = function registerBookmark(path: string): RegisterAction {
  return { type: ActionTypes.BOOKMARK_REGISTER, path };
};

export const deleteBookmark = function deleteBookmark(id: string): DeleteAction {
  return { type: ActionTypes.BOOKMARK_DELETE, id };
};
