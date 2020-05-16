import { ActionTypes } from "./types";
import { Actions } from "./actions";
import * as N from "@/types/natural-number";
import { Filer, FileWindow, FileList } from "@/generated/filer_pb";
import { ObjectEnum } from "@/utils";

export const Side = {
  Left: "Left",
  Right: "Right",
} as const;
export type Side = ObjectEnum<typeof Side>;

// state of type. Please redefine to what you want.
export type State = Readonly<{
  // filer object.
  filer: Filer | undefined;
  // 0-origin current cursor position.
  currentCursorPosition: {
    left: N.Type;
    right: N.Type;
  };
  // current side
  currentSide: Side;
}>;

/**
 * empty state that is used by this reducer
 */
export const emptyState: State = Object.freeze({
  filer: undefined,
  currentCursorPosition: {
    left: N.zero,
    right: N.zero,
  },
  currentSide: Side.Left,
});

const sideMap = <L, R>(
  state: State,
  left: (fw: FileWindow | undefined) => L,
  right: (fw: FileWindow | undefined) => R
): [L, R] | undefined => {
  const { filer } = state;

  if (!filer) {
    return undefined;
  }

  return [left(filer.getLeftFileWindow()), right(filer.getRightFileWindow())];
};

/**
 * reducer function for creator `actions.cursorDown`
 */
const cursorDown = (state: State): State => {
  const filer = state.filer;
  if (!filer) {
    return state;
  }

  let fileWindow: FileWindow | undefined;

  switch (state.currentSide) {
    case Side.Left:
      fileWindow = filer.getLeftFileWindow();
      break;
    case Side.Right:
      fileWindow = filer.getRightFileWindow();
      break;
  }
  const size = fileWindow?.getFileList()?.getItemsList();

  if (!size) {
    return state;
  }
  const naturalSize = N.max(N.zero, N.create(size.length - 1));

  return Object.freeze({
    ...state,
    currentCursorPosition: {
      left:
        state.currentSide === Side.Left
          ? N.min(naturalSize, N.inc(state.currentCursorPosition.left))
          : state.currentCursorPosition.left,
      right:
        state.currentSide === Side.Right
          ? N.min(naturalSize, N.inc(state.currentCursorPosition.right))
          : state.currentCursorPosition.right,
    },
  });
};

/**
 * reducer function for creator `actions.cursorUp`
 */
const cursorUp = (state: State): State => {
  return Object.freeze({
    ...state,
    currentCursorPosition: {
      left:
        state.currentSide === Side.Left ? N.dec(state.currentCursorPosition.left) : state.currentCursorPosition.left,
      right:
        state.currentSide === Side.Right ? N.dec(state.currentCursorPosition.right) : state.currentCursorPosition.right,
    },
  });
};

const changeSide = (state: State): State => {
  let nextSide = state.currentSide === Side.Left ? Side.Right : Side.Left;
  return Object.freeze({ ...state, currentSide: nextSide });
};

const reviseIndex = (fileList: FileList | undefined, currentCursor: N.Type) => {
  if (!fileList) {
    return N.zero;
  }

  const size = fileList.getItemsList().length;

  return N.min(currentCursor, N.create(size - 1));
};

const update = (state: State, filer: Filer): State => {
  const currentLeftLocation = state.filer?.getLeftFileWindow()?.getFileList()?.getLocation();
  const currentRightLocation = state.filer?.getRightFileWindow()?.getFileList()?.getLocation();

  const nextLeftFileList = filer?.getLeftFileWindow()?.getFileList();
  const nextRightFileList = filer?.getRightFileWindow()?.getFileList();
  const nextLeftLocation = nextLeftFileList?.getLocation();
  const nextRightLocation = nextRightFileList?.getLocation();

  const { left, right } = state.currentCursorPosition;

  return Object.freeze({
    ...state,
    filer,
    currentCursorPosition: {
      left: currentLeftLocation === nextLeftLocation ? reviseIndex(nextLeftFileList, left) : N.zero,
      right: currentRightLocation === nextRightLocation ? reviseIndex(nextRightFileList, right) : N.zero,
    },
  });
};

const updateFileWindow = (state: State, payload: { fileWindow: FileWindow; side: Side }) => {
  const { filer: oldFiler } = state;
  if (!oldFiler) {
    return state;
  }
  const filer = oldFiler.clone();
  let currentFileWindow: FileWindow | undefined;

  switch (payload.side) {
    case Side.Left:
      currentFileWindow = oldFiler.getLeftFileWindow();
      filer.setLeftFileWindow(payload.fileWindow);
      break;
    case Side.Right:
      currentFileWindow = oldFiler.getRightFileWindow();
      filer.setRightFileWindow(payload.fileWindow);
      break;
  }
  const sameLocation =
    currentFileWindow?.getFileList()?.getLocation() === payload.fileWindow.getFileList()?.getLocation();

  return Object.freeze({
    ...state,
    filer,
    currentCursorPosition: {
      left: payload.side === Side.Left && !sameLocation ? N.zero : state.currentCursorPosition.left,
      right: payload.side === Side.Right && !sameLocation ? N.zero : state.currentCursorPosition.right,
    },
  });
};

const focusItem = (state: State, payload: { itemId: string }): State => {
  const findIndex = (fw: FileList | undefined): number | undefined =>
    fw?.getItemsList()?.findIndex((v) => v.getId() === payload.itemId);
  const currentSide = state.currentSide;

  const ret = sideMap(
    state,
    (fw) => {
      const index = findIndex(fw?.getFileList());

      if (index !== undefined && currentSide === Side.Left) {
        return N.create(index);
      }
      return state.currentCursorPosition.left;
    },
    (fw) => {
      const index = findIndex(fw?.getFileList());

      if (index !== undefined && currentSide === Side.Right) {
        return N.create(index);
      }
      return state.currentCursorPosition.right;
    }
  );

  if (!ret) {
    return state;
  }
  const [leftIndex, rightIndex] = ret;

  return Object.freeze({
    ...state,
    currentCursorPosition: {
      left: leftIndex,
      right: rightIndex,
    },
  });
};

export const reducer = function reducer(state: State = emptyState, action: Actions): State {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return update(state, action.payload.filer);
    case ActionTypes.CURSOR_DOWN:
      return cursorDown(state);
    case ActionTypes.CURSOR_UP:
      return cursorUp(state);
    case ActionTypes.CHANGE_SIDE:
      return changeSide(state);
    case ActionTypes.UPDATE_FILE_WINDOW:
      return updateFileWindow(state, action.payload);
    case ActionTypes.FOCUS_ITEM:
      return focusItem(state, action.payload);
    default:
      return state;
  }
};
