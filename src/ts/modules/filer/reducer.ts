import { ActionTypes } from "./types";
import { Actions } from "./actions";
import * as N from "@/types/natural-number";
import { Filer, FileWindow, FileList, FileItemOrder, FileEvent, FileEventType } from "@/generated/filer_pb";
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

const updateItemOrders = (state: State, payload: { itemOrders: FileItemOrder[]; fileListId: string }) => {
  const { filer: oldFiler } = state;
  if (!oldFiler) {
    return state;
  }
  const filer = oldFiler.clone();
  let currentFileWindow: FileWindow | undefined;
  let side: Side | undefined;

  if (oldFiler.getLeftFileWindow()?.getFileList()?.getId() === payload.fileListId) {
    const fileList = oldFiler.getLeftFileWindow()?.getFileList();
    fileList?.setFileItemOrdersList(payload.itemOrders);
    currentFileWindow = oldFiler.getLeftFileWindow();

    currentFileWindow?.setFileList(fileList);

    if (currentFileWindow) {
      currentFileWindow.setFileList(fileList);
      filer.setLeftFileWindow(currentFileWindow);
    }
    side = Side.Left;
  } else if (oldFiler.getRightFileWindow()?.getFileList()?.getId() === payload.fileListId) {
    const fileList = oldFiler.getRightFileWindow()?.getFileList();
    fileList?.setFileItemOrdersList(payload.itemOrders);
    currentFileWindow = oldFiler.getLeftFileWindow();

    if (currentFileWindow) {
      currentFileWindow.setFileList(fileList);
      filer.setRightFileWindow(currentFileWindow);
    }
    side = Side.Right;
  }

  return Object.freeze({
    ...state,
    filer,
    currentCursorPosition: {
      left: side === Side.Left ? N.zero : state.currentCursorPosition.left,
      right: side === Side.Right ? N.zero : state.currentCursorPosition.right,
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

const applyEvents = (state: State, payload: { fileListId: string; fileEvents: FileEvent[] }): State => {
  const { fileListId, fileEvents } = payload;
  let targetFileWindow: FileWindow | undefined;
  let side: Side = Side.Left;

  if (state.filer?.getLeftFileWindow()?.getFileList()?.getId() === fileListId) {
    targetFileWindow = state.filer?.getLeftFileWindow();
    side = Side.Left;
  } else if (state.filer?.getRightFileWindow()?.getFileList()?.getId() === fileListId) {
    targetFileWindow = state.filer?.getRightFileWindow();
    side = Side.Right;
  }

  const targetFileList = targetFileWindow?.getFileList();
  const filer = state.filer?.clone();

  if (!targetFileWindow || !targetFileList || !filer) return state;

  fileEvents.forEach((event) => {
    switch (event.getEventType()) {
      case FileEventType.ADD:
        targetFileList.addItems(event.getFileItem());
        break;
      case FileEventType.DELETE:
        {
          const items = targetFileList.getItemsList().filter((v) => v.getId() !== event.getFileItem()?.getId());

          targetFileList.setItemsList(items);
        }
        break;
      case FileEventType.UPDATE:
        {
          const item = event.getFileItem();
          const items = targetFileList.getItemsList().map((v) => {
            if (!item) return v;
            if (v.getId() !== item.getId()) return v;

            return item;
          });

          targetFileList.setItemsList(items);
        }
        break;
      default:
        break;
    }
  });

  targetFileWindow.setFileList(targetFileList);
  if (side === Side.Left) {
    filer.setLeftFileWindow(targetFileWindow);
  } else if (side === Side.Right) {
    filer.setRightFileWindow(targetFileWindow);
  }

  return { ...state, filer };
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
    case ActionTypes.UPDATE_ITEM_ORDERS:
      return updateItemOrders(state, action.payload);
    case ActionTypes.FOCUS_ITEM:
      return focusItem(state, action.payload);
    case ActionTypes.APPLY_EVENTS:
      return applyEvents(state, action.payload);
    default:
      return state;
  }
};
