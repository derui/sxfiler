import { h } from "preact";
import * as MainContainer from "./components/container/main-container";
import { State } from "@/modules";
import { ThemeContext } from "./theme";
import { getCurrentTheme } from "./modules/configuration/selectors";

export type Props = {
  state: State;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const theme = getCurrentTheme(state.configuration);

  return (
    <ThemeContext.Provider value={theme}>
      <MainContainer.Component state={state} />
    </ThemeContext.Provider>
  );
};
