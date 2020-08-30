import { State } from "./reducer";

export const selectColorPairs = (state: State) => {
  return state.theme.getColorPairsList().reduce((obj, color) => {
    obj[color.getName()] = color.getHexColor();
    return obj;
  }, {} as { [p: string]: string });
};
