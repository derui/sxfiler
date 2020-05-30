import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Configuration } from "@/generated/configuration_pb";
import { Section } from "@/configurations/types";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const close = () => {
  return { type: ActionTypes.CLOSE, payload: {} };
};
export const open = () => {
  return { type: ActionTypes.OPEN, payload: {} };
};
export const selectSection = (section: Section) => {
  return {
    type: ActionTypes.SELECT_SECTION,
    payload: { section },
  };
};
export const update = (config: Configuration[]) => {
  return {
    type: ActionTypes.UPDATE,
    payload: { config },
  };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
close,
open,
selectSection,
update,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
