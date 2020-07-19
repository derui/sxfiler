import { createSelector } from "reselect";
import { State } from "./reducer";
import { defaultTheme } from "./default-theme";

export const selectTheme = (state: State, themeName: string) => state.themes.find((v) => v.getName() === themeName);

export const selectColorPairs = (state: State, themeName: string) => {
  const theme = selectTheme(state, themeName) || defaultTheme();

  return theme.getColorCodesList().reduce((obj, color) => {
    obj[color.getName()] = color.getHexColor();
    return obj;
  }, {} as { [p: string]: string });
};
