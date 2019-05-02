import { Keymap } from "../domains/keymap";
import { AppAction } from "./type";

export enum ActionTypes {
  updateKeymap = "update_keymap",
}

type UpdateAction = AppAction<ActionTypes.updateKeymap, { keymap: Keymap }>;

export type Actions = UpdateAction;

/**
   update the key map
 */
const updateKeymap = (keymap: Keymap): UpdateAction => {
  return { type: ActionTypes.updateKeymap, keymap };
};

export const actions = { updateKeymap };
