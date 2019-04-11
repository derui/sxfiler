import { AppAction } from "./type";
import { Keymap } from "../domains/keymap";

export enum ActionTypes {
  getKeymap = "keymap_get",
}

type GetKeymapAction = AppAction<
  ActionTypes.getKeymap,
  {
    payload: {
      keymap: Keymap;
    };
  }
>;

export type Actions = GetKeymapAction;

const getKeymap = (args: { keymap: Keymap }): GetKeymapAction => {
  return { type: ActionTypes.getKeymap, payload: { ...args } };
};

export const actions = { getKeymap };
