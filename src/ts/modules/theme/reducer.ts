import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { ColorTheme } from "@/generated/theme_pb";

// state of type. Please redefine to what you want.
export type State = {
  theme: ColorTheme;
};

export const emptyState: State = { theme: new ColorTheme() };

const updateTheme = (state: State, theme: ColorTheme) => {
  return { ...state, theme };
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return updateTheme(state, action.payload.theme);
    default:
      return state;
  }
};
