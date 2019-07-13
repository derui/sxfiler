import { Keymap } from "@/domains/keymap";
import { AppAction, ActionTypes } from "./type";

type UpdateAction = AppAction<ActionTypes.KEYMAP_UPDATE, { keymap: Keymap }>;

export type Actions = UpdateAction;

/**
   update the key map
 */
const updateKeymap = (keymap: Keymap): UpdateAction => {
  return { type: ActionTypes.KEYMAP_UPDATE, keymap };
};

export const actions = { updateKeymap };
