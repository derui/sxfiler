import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Keymap } from "@/generated/keymap_pb";
import { UIContext } from "@/types/ui-context";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const removeContexts = function removeContexts(contexts: UIContext[]) {
  return { type: ActionTypes.REMOVE_CONTEXTS, payload: { contexts } };
};

export const addContexts = function addContexts(contexts: UIContext[]) {
  return { type: ActionTypes.ADD_CONTEXTS, payload: { contexts } };
};

export const update = function update(keymap: Keymap) {
  return { type: ActionTypes.UPDATE, payload: { keymap } };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
  removeContexts,
  addContexts,
  update,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
