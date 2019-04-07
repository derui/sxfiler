import { AppAction } from "./type";
import { Filer } from "../domains/filer";
import { Side } from "../states/file-list";

export enum ActionTypes {
  initialize = "filer_initialize",
  updateFiler = "filer_update_filer",
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

type UpdateFilerAction = AppAction<
  ActionTypes.updateFiler,
  {
    payload: {
      filer: Filer;
      side: Side;
    };
  }
>;
export type Actions = InitializeAction | UpdateFilerAction;

const initialize = (args: { left: Filer; right: Filer }): InitializeAction => {
  return { type: ActionTypes.initialize, payload: { ...args } };
};

/**
 * Update filer on the side.
 */
const update = (args: { filer: Filer; side: Side }): UpdateFilerAction => {
  return { type: ActionTypes.updateFiler, payload: { ...args } };
};

export const actions = { initialize, update };
