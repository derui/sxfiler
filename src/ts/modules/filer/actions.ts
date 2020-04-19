import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Filer, FileWindow } from "@/generated/filer_pb";
import { Side } from "./reducer";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const updateFileWindow = (fileWindow: FileWindow, side: Side) => {
  return { type: ActionTypes.UPDATE_FILE_WINDOW, payload: { fileWindow, side } };
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
updateFileWindow,
changeSide,
update,
cursorDown,
cursorUp,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
