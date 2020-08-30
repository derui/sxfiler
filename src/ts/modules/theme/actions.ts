import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { ColorTheme } from "@/generated/theme_pb";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const invalidTheme = (errors: string[]) => {
  return { type: ActionTypes.INVALID_THEME, payload: { errors } };
};

export const update = (theme: ColorTheme) => {
  return { type: ActionTypes.UPDATE, payload: { theme } };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
invalidTheme,
update,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
