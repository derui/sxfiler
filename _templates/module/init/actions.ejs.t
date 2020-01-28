---
to: src/ts/modules/<%= name %>/actions.ts
---
import {ActionsType} from "../type";
import {ActionTypes} from "./types";

// implememt action. Use command `hygen module add:action [name of action]` to add template into this place.
//#ACTION INSERTION INDICATOR

// Do not delete this comment below.
// prettier-ignore
export const actions = {
};

// exporting all actions
export type Actions = ActionsType<typeof ActionTypes, typeof actions>;