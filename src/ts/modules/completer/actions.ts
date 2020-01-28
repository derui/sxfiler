import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Candidate } from "@/generated/completer_pb";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const close = () => {
  return { type: ActionTypes.CLOSE, payload: {} };
};

export const open = (title: string) => {
  return { type: ActionTypes.OPEN, payload: { title } };
};

export const updateCandidates = (candidates: Candidate[]) => {
  return { type: ActionTypes.UPDATE_CANDIDATES, payload: { candidates } };
};

export const cursorDown = () => {
  return { type: ActionTypes.CURSOR_DOWN, payload: {} };
};

export const cursorUp = () => {
  return { type: ActionTypes.CURSOR_UP, payload: {} };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
close,
open,
updateCandidates,
cursorDown,
cursorUp,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
