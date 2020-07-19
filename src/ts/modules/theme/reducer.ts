import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { Theme } from "@/generated/theme_pb";

// state of type. Please redefine to what you want.
export type State = {
  themes: Theme[];
};

export const defaultTheme = (() => {
  const theme = new Theme();
  return theme;
})();

export const emptyState: State = { themes: [] };

const updateThemes = (state: State, themes: Theme[]) => {
  return { ...state, themes };
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE_LIST:
      return updateThemes(state, action.payload.themes);
    default:
      return state;
  }
};
