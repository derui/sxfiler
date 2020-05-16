import { ActionsType } from "../type";
import { ActionTypes } from "./types";
import { LogEvents } from "./reducer";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR
export const send = (events: LogEvents[]) => {
  return { type: ActionTypes.SEND, payload: events };
};

// Do not delete this comment below.
// prettier-ignore
export const actions = {
send,
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;
