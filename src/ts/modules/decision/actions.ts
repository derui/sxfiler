import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { FileItem } from "@/generated/filer_pb";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const cancel = () => {
  return { type: ActionTypes.CANCEL, payload: {} };
};
export const reset = () => {
  return { type: ActionTypes.RESET, payload: {} };
};
export const updateNewName = (newName: string) => {
  return { type: ActionTypes.UPDATE_NEW_NAME, payload: { newName } };
};

export const selectPreviousAction = () => {
  return { type: ActionTypes.SELECT_PREVIOUS_ACTION, payload: {} };
};

export const selectNextAction = () => {
  return { type: ActionTypes.SELECT_NEXT_ACTION, payload: {} };
};

export const finish = () => {
  return { type: ActionTypes.FINISH, payload: {} };
};

export const requireDecisionForMove = (processId: string, item: FileItem) => {
  return { type: ActionTypes.REQUIRE_DECISION_FOR_MOVE, payload: { processId, item } };
};

export const requireDecisionForDelete = (processId: string, item: FileItem) => {
  return { type: ActionTypes.REQUIRE_DECISION_FOR_DELETE, payload: { processId, item } };
};

export const requireDecisionForCopy = (processId: string, item: FileItem) => {
  return { type: ActionTypes.REQUIRE_DECISION_FOR_COPY, payload: { processId, item } };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
cancel,
reset,
updateNewName,
selectPreviousAction,
selectNextAction,
finish,
requireDecisionForMove,
requireDecisionForDelete,
requireDecisionForCopy,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
