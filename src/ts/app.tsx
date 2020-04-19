import { h, Fragment } from "preact";
import * as MainContainer from "./components/container/main-container";
import { State } from "@/modules";
import { ThemeContext } from "./theme";
import { getCurrentTheme } from "./modules/configuration/selectors";
import { ModalThemeUpdater } from "./modal-theme-updater";

export type Props = {
  state: State;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const theme = getCurrentTheme(state.configuration);

  return (
    <Fragment>
      <ThemeContext.Provider value={theme}>
        <ModalThemeUpdater />
      </ThemeContext.Provider>

      <ThemeContext.Provider value={theme}>
        <MainContainer.Component state={state} />
      </ThemeContext.Provider>
    </Fragment>
  );
};
