import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Theme } from "@/generated/theme_pb";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const invalidTheme = (errors: string[]) => {
  return { type: ActionTypes.INVALID_THEME, payload: { errors } };
};

export const updateList = (themes: Theme[]) => {
  return { type: ActionTypes.UPDATE_LIST, payload: { themes } };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
invalidTheme,
updateList,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
