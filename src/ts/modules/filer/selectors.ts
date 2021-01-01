import { createSelector } from "reselect";
import { State, Side } from "./reducer";
import { FileList, FileWindow, Side as PbSide } from "@/generated/filer_pb";

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
  fileWindow: FileWindow
): FileList | undefined {
  const fileList = fileWindow.getFileList();

  if (!fileList) {
    return undefined;
  }

  const orders = fileList.getFileItemOrdersList().reduce((obj , v) => {
    obj[v.getFileId()] = v.getSortLevel();
    return obj;
  }, {} as {[key:string]: number});

  const sortedItems = fileList.getItemsList().sort((v1, v2) => {
    const v1Order = orders[v1.getId()];
    const v2Order = orders[v2.getId()];
    const v = Math.abs(v1Order - v2Order);

    if ( v < Number.EPSILON) {
      return -v;
    } else {
      return v;
    }
  });

  const newFileList = fileList.clone();
  newFileList.setItemsList(sortedItems);

  return newFileList;
};

/**
 * select specified side file list
 */
export const leftSideFileListSelector = (state: State) => {
  const fileWindow =  state.filer?.getLeftFileWindow() ;

  if (!fileWindow) {
    return null;
  }

  return getSortedFileList(fileWindow);
};
export const rightSideFileListSelector = (state: State) => {
  const fileWindow =  state.filer?.getRightFileWindow() ;

  if (!fileWindow) {
    return null;
  }

  return getSortedFileList(fileWindow);
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
