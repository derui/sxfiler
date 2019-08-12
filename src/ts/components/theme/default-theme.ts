// styled-components.ts
import * as styledComponents from "styled-components";

export interface Theme {
  baseFontSize: string;
  baseBorderRadius: string;
  headerShadow: string;
  boxShadow: string;
  colors: {
    base03: string;
    base02: string;
    base01: string;
    base00: string;
    base0: string;
    base1: string;
    base2: string;
    base3: string;
    yellow: string;
    orange: string;
    red: string;
    magenta: string;
    violet: string;
    blue: string;
    cyan: string;
    green: string;
  };
  spaces: {
    large: string;
    base: string;
    nano: string;
    small: string;
  };
}

export const Theme = {
  baseFontSize: "13.5px",
  baseBorderRadius: "4px",
  headerShadow: "0px 7px 3px rgba(0, 0, 0, 0.2)",
  boxShadow: "4px 4px 7px rgba(0, 0, 0, 0.2), -4px 0px 7px rgba(0, 0, 0, 0.2)",

  colors: {
    /* color schema of solalized-dark (from http://ethanschoonover.com/solarized) */
    base03: "#002b36",
    base02: "#073642",
    base01: "#586e75",
    base00: "#657b83",
    base0: "#839496",
    base1: "#93a1a1",
    base2: "#eee8d5",
    base3: "#fdf6e3",
    yellow: "#b58900",
    orange: "#cb4b16",
    red: "#dc322f",
    magenta: "#d33682",
    violet: "#6c71c4",
    blue: "#268bd2",
    cyan: "#2aa198",
    green: "#859900",
  },
  spaces: {
    large: "16px",
    base: "8px",
    small: "4px",
    nano: "2px",
  },
};

const { default: styled, css, ThemeProvider } = styledComponents as styledComponents.ThemedStyledComponentsModule<
  Theme
>;

export { styled, css, ThemeProvider };
