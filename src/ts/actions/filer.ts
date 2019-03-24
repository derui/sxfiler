import { AppAction } from "./type";
import { Filer } from "../domains/filer";

export enum ActionTypes {
  initialize = "filer_initialize",
}

type InitializeAction = AppAction<
  ActionTypes.initialize,
  {
    payload: {
      left: Filer;
      right: Filer;
    };
  }
>;
export type Actions = InitializeAction;

const initialize = (args: { left: Filer; right: Filer }): InitializeAction => {
  return { type: ActionTypes.initialize, payload: args };
};

export const actions = { initialize };
