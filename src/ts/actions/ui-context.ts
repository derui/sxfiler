import { AppAction } from "./type";
import { Keymap } from "../domains/keymap";

export enum ActionTypes {
  enableFileTree = "uicontext_enable_file_tree",
  enableSuggestion = "uicontext_enable_suggestion",
}

type EnableFileTreeAction = AppAction<ActionTypes.enableFileTree, { keymap: Keymap }>;
type EnableSuggestionAction = AppAction<ActionTypes.enableSuggestion, { keymap: Keymap }>;

export type Actions = EnableFileTreeAction | EnableSuggestionAction;

const enableFileTree = ({ keymap }: { keymap: Keymap }): EnableFileTreeAction => {
  return { type: ActionTypes.enableFileTree, keymap: keymap };
};

const enableSuggestion = ({ keymap }: { keymap: Keymap }): EnableSuggestionAction => {
  return { type: ActionTypes.enableSuggestion, keymap };
};

export const actions = { enableFileTree, enableSuggestion };
