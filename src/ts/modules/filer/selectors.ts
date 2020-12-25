import { createSelector } from "reselect";
import { State, Side } from "./reducer";
import { FileList, FileWindow, Side as PbSide } from "@/generated/filer_pb";
import { SortTypeMap } from "@/generated/types_pb";

// helper selector
const currentIndexSelector = (state: State) => {
  switch (state.currentSide) {
    case Side.Left:
      return state.currentCursorPosition.left;
    case Side.Right:
      return state.currentCursorPosition.right;
  }
};
const getCurrentSide = (state: State) => state.currentSide;
export const currentSideItemsSelector = (state: State) => {
  switch (state.currentSide) {
    case Side.Left:
      return state.filer?.getLeftFileWindow()?.getFileList()?.getItemsList() || [];
    case Side.Right:
      return state.filer?.getRightFileWindow()?.getFileList()?.getItemsList() || [];
  }
};

const getSortedFileList = function getSortedFileList(
  sortType: SortTypeMap,
  fileWindow: FileWindow
): FileList | undefined {
  const fileList = fileWindow.getFileList();

  if (!fileList) {
    return undefined;
  }

  const orders = fileList.getFileItemOrdersList();
};

/**
 * select specified side file list
 */
export const leftSideFileListSelector = (state: State) => {
  return state.filer?.getLeftFileWindow()?.getFileList();
};
export const rightSideFileListSelector = (state: State) => {
  return state.filer?.getRightFileWindow()?.getFileList();
};
export const leftSideCursorPositionSelector = (state: State) => state.currentCursorPosition.left;
export const rightSideCursorPositionSelector = (state: State) => state.currentCursorPosition.right;

/**
 * get current side as type in generated type
 */
export const getCurrentSideInPb = createSelector(getCurrentSide, (side) => {
  switch (side) {
    case Side.Left:
      return PbSide.LEFT;
    case Side.Right:
      return PbSide.RIGHT;
  }
});

/**
 * select current focused item if available
 */
export const currentFocusingItemSelector = createSelector(
  currentIndexSelector,
  currentSideItemsSelector,
  getCurrentSideInPb,
  (index, items, side) => {
    if (items.length === 0) {
      return undefined;
    }

    return { item: items[index.value], side };
  }
);

export const currentSideMarkedItems = createSelector(currentSideItemsSelector, (items) => {
  return items.filter((v) => v.getMarked());
});
