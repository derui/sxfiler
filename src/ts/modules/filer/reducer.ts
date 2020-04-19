import { ActionTypes } from "./types";
import { Actions } from "./actions";
import * as N from "@/types/natural-number";
import { Filer, FileWindow } from "@/generated/filer_pb";

export enum Side {
  Left = "Left",
  Right = "Right",
}

// state of type. Please redefine to what you want.
export type State = Readonly<{
  // filer object.
  filer: Filer | undefined;
  // 0-origin current cursor position.
  currentCursorPosition: {
    left: N.NaturalNumber;
    right: N.NaturalNumber;
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
    left: N.create(0),
    right: N.create(0),
  },
  currentSide: Side.Left,
});

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
  const naturalSize = N.create(size.length - 1);

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

const update = (state: State, filer: Filer): State => {
  const currentLeftLocation = state.filer?.getLeftFileWindow()?.getFileList()?.getLocation();
  const currentRightLocation = state.filer?.getRightFileWindow()?.getFileList()?.getLocation();

  const nextLeftLocation = filer.getLeftFileWindow()?.getFileList()?.getLocation();
  const nextRightLocation = filer.getRightFileWindow()?.getFileList()?.getLocation();

  return Object.freeze({
    ...state,
    filer,
    currentCursorPosition: {
      left: currentLeftLocation === nextLeftLocation ? state.currentCursorPosition.left : N.create(0),
      right: currentRightLocation === nextRightLocation ? state.currentCursorPosition.right : N.create(0),
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
      left: payload.side === Side.Left && !sameLocation ? N.create(0) : state.currentCursorPosition.left,
      right: payload.side === Side.Right && !sameLocation ? N.create(0) : state.currentCursorPosition.right,
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
    default:
      return state;
  }
};
