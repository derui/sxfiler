import { AppAction, ActionTypes } from "./type";
import { Filer } from "@/domains/filer";
import { Side } from "@/states/file-list";

type SelectAction = AppAction<ActionTypes.FILER_SELECT, { side: Side; itemId: string }>;
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

export type Actions = ReloadAction | UpdateFilerAction | ChangeSideAction | LoadFilerAction | SelectAction;

export const reload = function reload(args: { filers: [Filer, Filer] }): ReloadAction {
  return { type: ActionTypes.FILER_RELOAD, payload: { ...args } };
};

/**
 * Update filer
 */
export const update = function update(args: { filer: Filer }): UpdateFilerAction {
  return { type: ActionTypes.FILER_UPDATE, payload: { ...args } };
};

/**
 * Load or get filer on the side.
 */
export const load = function load(args: { filer: Filer }): LoadFilerAction {
  return { type: ActionTypes.FILER_LOAD, payload: { ...args } };
};

/**
   change current side
*/
export const changeSide = function changeSide(): ChangeSideAction {
  return { type: ActionTypes.FILER_CHANGE_SIDE };
};

export const select = function select(side: Side, itemId: string): SelectAction {
  return { type: ActionTypes.FILER_SELECT, side, itemId };
};
