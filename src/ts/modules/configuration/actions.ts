import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { Configuration } from "@/generated/configuration_pb";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const update = (config: Configuration[]) => {
  return {
    type: ActionTypes.UPDATE,
    payload: { config },
  };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
update,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
