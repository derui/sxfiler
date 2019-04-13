import { AppAction } from "./type";
import { Filer } from "../domains/filer";
import { Side } from "../states/file-list";

export enum ActionTypes {
  initialize = "filer_initialize",
  updateFiler = "filer_update_filer",
  changeSide = "filer_change_side",
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

type ChangeSideAction = AppAction<ActionTypes.changeSide>;

export type Actions = InitializeAction | UpdateFilerAction | ChangeSideAction;

const initialize = (args: { left: Filer; right: Filer }): InitializeAction => {
  return { type: ActionTypes.initialize, payload: { ...args } };
};

/**
 * Update filer on the side.
 */
const update = (args: { filer: Filer; side: Side }): UpdateFilerAction => {
  return { type: ActionTypes.updateFiler, payload: { ...args } };
};

/**
   change current side
*/
const changeSide = (): ChangeSideAction => {
  return { type: ActionTypes.changeSide };
};

export const actions = { initialize, update, changeSide };
