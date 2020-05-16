import { h } from "preact";
import classnames from "classnames";

import * as FilerContainer from "./filer-container";
import * as LogViewerContainer from "./log-viewer-container";
import * as CompleterContainer from "./completer-container";

import { State } from "@/modules";
import * as DecisionModalContainer from "./decision-modal-container";
import { useTheme } from "@/theme";

export type Props = {
  state: State;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const { filer } = state;
  const theme = useTheme();

  return (
    <div
      class={classnames({
        "main-container__root ": true,
        [theme || ""]: !!theme,
      })}
      data-testid="mainContainer"
    >
      <FilerContainer.Component state={filer} />
      <DecisionModalContainer.Component state={state} />
      <LogViewerContainer.Component state={state} />
      <CompleterContainer.Component state={state} />
    </div>
  );
};
