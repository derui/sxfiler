import * as React from "react";
import { Theme, ThemeProvider } from "@/components/theme";
import * as MainContainer from "./components/container/main-container";
import { AppState } from "./states";

export type Props = {
  state: AppState;
};

export const Component: React.FC<Props> = ({ state }) => (
  <ThemeProvider theme={Theme}>
    <MainContainer.Component state={state} />
  </ThemeProvider>
);
