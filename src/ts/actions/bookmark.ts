import { AppAction, ActionTypes } from "./type";
import { Bookmark } from "@/domains/bookmark";

type RegisterAction = AppAction<ActionTypes.BOOKMARK_REGISTER, { bookmark: Bookmark }>;
type DeleteAction = AppAction<ActionTypes.BOOKMARK_DELETE, { bookmark: Bookmark }>;
type UpdateAction = AppAction<ActionTypes.BOOKMARK_UPDATE, { bookmarks: Bookmark[] }>;

export type Actions = UpdateAction | DeleteAction | RegisterAction;

/**
   update the key map
 */
export const updateBookmarks = function updateBookmarks(bookmarks: Bookmark[]): UpdateAction {
  return { type: ActionTypes.BOOKMARK_UPDATE, bookmarks };
};

export const registerBookmark = function registerBookmark(bookmark: Bookmark): RegisterAction {
  return { type: ActionTypes.BOOKMARK_REGISTER, bookmark };
};

export const deleteBookmark = function deleteBookmark(bookmark: Bookmark): DeleteAction {
  return { type: ActionTypes.BOOKMARK_DELETE, bookmark };
};
