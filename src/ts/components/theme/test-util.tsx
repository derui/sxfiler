import * as React from "react";
import { Theme, ThemeProvider } from "./default-theme";

/**
   wrap element with default theme.
 */
export const wrap = function wrap(e: React.ReactElement) {
  return <ThemeProvider theme={Theme}>{e}</ThemeProvider>;
};
