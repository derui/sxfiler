import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { FileItemOrder, Filer, FileWindow } from "@/generated/filer_pb";
import { Side } from "./reducer";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const updateItemOrders = (fileListId: string, itemOrders: FileItemOrder[]) => {
  return { type: ActionTypes.UPDATE_ITEM_ORDERS, payload: { fileListId, itemOrders } };
};

export const focusItem = (itemId: string) => {
  return { type: ActionTypes.FOCUS_ITEM, payload: { itemId } };
};

export const changeSide = function changeSide() {
  return { type: ActionTypes.CHANGE_SIDE, payload: {} };
};

export const update = function update(filer: Filer) {
  return { type: ActionTypes.UPDATE, payload: { filer } };
};

export const cursorDown = function cursorDown() {
  return { type: ActionTypes.CURSOR_DOWN, payload: {} };
};

export const cursorUp = function cursorUp() {
  return { type: ActionTypes.CURSOR_UP, payload: {} };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
updateItemOrders,
focusItem,
changeSide,
update,
cursorDown,
cursorUp,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
