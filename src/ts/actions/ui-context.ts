import { AppAction } from "./type";
import { Keymap } from "../domains/keymap";

export enum ActionTypes {
  enableFileTree = "uicontext_enable_file_tree",
  enableSuggestion = "uicontext_enable_preview",
}

type EnableFileTreeAction = AppAction<ActionTypes.enableFileTree, { keymap: Keymap }>;
type EnablePreviewAction = AppAction<ActionTypes.enableSuggestion, { keymap: Keymap }>;

export type Actions = EnableFileTreeAction | EnablePreviewAction;

const enableFileTree = ({ keymap }: { keymap: Keymap }): EnableFileTreeAction => {
  return { type: ActionTypes.enableFileTree, keymap: keymap };
};

const enablePreview = ({ keymap }: { keymap: Keymap }): EnablePreviewAction => {
  return { type: ActionTypes.enableSuggestion, keymap };
};

export const actions = { enableFileTree, enablePreview };
