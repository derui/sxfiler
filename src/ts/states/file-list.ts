import { Filer } from "@/domains/filer";
import { FileItem } from "@/domains/file-item";
import { Bookmark } from "@/domains/bookmark";

export enum Side {
  Left = "left",
  Right = "right",
}

export const swapSide = (side: Side): Side => {
  switch (side) {
    case Side.Left:
      return Side.Right;
    case Side.Right:
      return Side.Left;
  }
};

export type State = {
  left?: Filer;
  // filer of right side
  right?: Filer;
  // current selected side
  currentSide: Side;
  bookmarks: { [key: string]: Bookmark };
};

/** factory function create empty state */
export const empty = function(): State {
  return {
    currentSide: Side.Left,
    bookmarks: {},
  };
};

/**
   Initialize a state with filers
 */
export const initialize = function(state: State, { left, right }: { left: Filer; right: Filer }): State {
  return {
    ...state,
    left,
    right,
  };
};

/**
 * Get the filer on the side
 *
 * @param state The state of file list
 * @param side the side to get filer from state
 */
export const filerOnSide = function(state: State, side: Side): Filer | undefined {
  switch (side) {
    case Side.Left:
      return state.left;
    case Side.Right:
      return state.right;
  }
};

/**
 * Compare specific position is current or not.
 *
 * @param state state to operate
 * @param pos check position
 * @return state has same position
 */
export const isCurrent = function(state: State, pos: Side): boolean {
  return state.currentSide === pos;
};

/**
 * Get side swapped state.
 * @param state operation target
 * @return side swapped instance
 */
export const fellowPosition = function(state: State): State {
  switch (state.currentSide) {
    case Side.Left:
      return { ...state, currentSide: Side.Right };
    case Side.Right:
      return { ...state, currentSide: Side.Left };
  }
};

/**
 * Get current focusing node
 *
 * @param state The state of file list
 */
export const currentFocusingNode = function(state: State): FileItem | undefined {
  if (!state.left || !state.right) {
    return undefined;
  }

  const side = state.currentSide;

  switch (side) {
    case Side.Left:
      return state.left.currentFileItem;
    case Side.Right:
      return state.right.currentFileItem;
  }
};

/**
   update bookmarks of the state
 */
export const updateBookmarks = function updateBookmarks(bookmarks: Bookmark[]) {
  return (state: State): State => {
    return { ...state, bookmarks: bookmarks.reduce((accum, v) => ({ ...accum, [v.id]: v }), {}) };
  };
};

/**
   register a bookmark to the state
 */
export const registerBookmark = function registerBookmark(bookmark: Bookmark) {
  return (state: State): State => {
    return { ...state, bookmarks: { ...state.bookmarks, [bookmark.id]: bookmark } };
  };
};

/**
   delete a bookmark from the state
 */
export const deleteBookmark = function deleteBookmark(bookmark: Bookmark) {
  return (state: State): State => {
    const bookmarks = { ...state.bookmarks };

    delete bookmarks[bookmark.id];
    return { ...state, bookmarks };
  };
};

/**
   find the bookmark for the file item
 */
export const findBookmark = function findBookmark(fileItem: FileItem) {
  return (state: State): Bookmark | undefined => Object.values(state.bookmarks).find(v => v.path === fileItem.fullPath);
};
