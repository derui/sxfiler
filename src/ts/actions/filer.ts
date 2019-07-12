import { AppAction, ActionTypes } from "./type";
import { Filer } from "../domains/filer";

type ReloadAction = AppAction<
  ActionTypes.FILER_RELOAD,
  {
    payload: {
      filers: [Filer, Filer];
    };
  }
>;

type UpdateFilerAction = AppAction<
  ActionTypes.FILER_UPDATE,
  {
    payload: {
      filer: Filer;
    };
  }
>;

// The action to load or get a filer from server
type LoadFilerAction = AppAction<
  ActionTypes.FILER_LOAD,
  {
    payload: {
      filer: Filer;
    };
  }
>;

type ChangeSideAction = AppAction<ActionTypes.FILER_CHANGE_SIDE>;

export type Actions = ReloadAction | UpdateFilerAction | ChangeSideAction | LoadFilerAction;

const reload = (args: { filers: [Filer, Filer] }): ReloadAction => {
  return { type: ActionTypes.FILER_RELOAD, payload: { ...args } };
};

/**
 * Update filer
 */
const update = (args: { filer: Filer }): UpdateFilerAction => {
  return { type: ActionTypes.FILER_UPDATE, payload: { ...args } };
};

/**
 * Load or get filer on the side.
 */
const load = (args: { filer: Filer }): LoadFilerAction => {
  return { type: ActionTypes.FILER_LOAD, payload: { ...args } };
};

/**
   change current side
*/
const changeSide = (): ChangeSideAction => {
  return { type: ActionTypes.FILER_CHANGE_SIDE };
};

export const actions = { update, changeSide, load, reload };
