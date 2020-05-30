import { h, Fragment } from "preact";
import * as MainContainer from "./components/container/main-container";
import { State } from "@/modules";
import { ThemeContext } from "./theme";
import { ModalThemeUpdater } from "./modal-theme-updater";
import { itemKeys } from "./configurations";
import { selectItem } from "./modules/configuration/selectors";

export type Props = {
  state: State;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const theme = selectItem(state.configuration, itemKeys.general.theme.currentTheme);

  if (!theme) {
    return null;
  }

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
