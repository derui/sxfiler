import { AppAction } from "./type";
import { Filer } from "../domains/filer";

export enum ActionTypes {
  updateFiler = "filer_update_filer",
  loadFiler = "filer_load_filer",
  reload = "filer_reload",
  changeSide = "filer_change_side",
}

type ReloadAction = AppAction<
  ActionTypes.reload,
  {
    payload: {
      filers: [Filer, Filer];
    };
  }
>;

type UpdateFilerAction = AppAction<
  ActionTypes.updateFiler,
  {
    payload: {
      filer: Filer;
    };
  }
>;

// The action to load or get a filer from server
type LoadFilerAction = AppAction<
  ActionTypes.loadFiler,
  {
    payload: {
      filer: Filer;
    };
  }
>;

type ChangeSideAction = AppAction<ActionTypes.changeSide>;

export type Actions = ReloadAction | UpdateFilerAction | ChangeSideAction | LoadFilerAction;

const reload = (args: { filers: [Filer, Filer] }): ReloadAction => {
  return { type: ActionTypes.reload, payload: { ...args } };
};

/**
 * Update filer
 */
const update = (args: { filer: Filer }): UpdateFilerAction => {
  return { type: ActionTypes.updateFiler, payload: { ...args } };
};

/**
 * Load or get filer on the side.
 */
const load = (args: { filer: Filer }): LoadFilerAction => {
  return { type: ActionTypes.loadFiler, payload: { ...args } };
};

/**
   change current side
*/
const changeSide = (): ChangeSideAction => {
  return { type: ActionTypes.changeSide };
};

export const actions = { update, changeSide, load, reload };
