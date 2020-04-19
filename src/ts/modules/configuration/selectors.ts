import { createSelector } from "reselect";
import { State } from "./reducer";

export const getCurrentTheme = (state: State) => {
  const theme = state.configuration?.currentTheme;
  if (!theme) {
    return "theme__default";
  }
  return `theme__${theme}`;
};
