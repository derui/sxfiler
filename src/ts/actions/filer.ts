import { AppAction } from "./type";
import { Filer } from "../domains/filer";
import { Side } from "../states/file-list";

export enum ActionTypes {
  initialize = "filer_initialize",
  updateFiler = "filer_update_filer",
  loadFiler = "filer_load_filer",
  reload = "filer_reload",
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

type ReloadAction = AppAction<
  ActionTypes.reload,
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

// The action to load or get a filer from server
type LoadFilerAction = AppAction<
  ActionTypes.loadFiler,
  {
    payload: {
      filer: Filer;
      side: Side;
    };
  }
>;

type ChangeSideAction = AppAction<ActionTypes.changeSide>;

export type Actions = InitializeAction | UpdateFilerAction | ChangeSideAction | LoadFilerAction;

const initialize = (args: { left: Filer; right: Filer }): InitializeAction => {
  return { type: ActionTypes.initialize, payload: { ...args } };
};

const reload = (args: { left: Filer; right: Filer }): ReloadAction => {
  return { type: ActionTypes.reload, payload: { ...args } };
};

/**
 * Update filer on the side.
 */
const update = (args: { filer: Filer; side: Side }): UpdateFilerAction => {
  return { type: ActionTypes.updateFiler, payload: { ...args } };
};

/**
 * Load or get filer on the side.
 */
const load = (args: { filer: Filer; side: Side }): LoadFilerAction => {
  return { type: ActionTypes.loadFiler, payload: { ...args } };
};

/**
   change current side
*/
const changeSide = (): ChangeSideAction => {
  return { type: ActionTypes.changeSide };
};

export const actions = { initialize, update, changeSide, load, reload };
