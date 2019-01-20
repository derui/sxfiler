import { AppAction } from "./type";

export enum ActionTypes {
  enableFileTree = "uicontext_enable_file_tree",
  enablePreview = "uicontext_enable_preview",
}

type EnableFileTreeAction = AppAction<ActionTypes.enableFileTree>;
type EnablePreviewAction = AppAction<ActionTypes.enablePreview>;

export type Actions = EnableFileTreeAction | EnablePreviewAction;

const enableFileTree = (): EnableFileTreeAction => {
  return { type: ActionTypes.enableFileTree };
};

const enablePreview = (): EnablePreviewAction => {
  return { type: ActionTypes.enablePreview };
};

export const actions = { enableFileTree, enablePreview };
