import * as React from "react";

import { styled } from "@/components/theme";
import * as Element from "@/components/ui/element";
import * as FileListContainer from "./file-list-container";
import * as NotificationContainer from "./notification-container";
import * as LogViewerContainer from "./log-viewer-container";
import * as HistorySelectorContainer from "./history-selector-container";
import * as FinderContainer from "./finder-container";
import * as CompleterContainer from "./completer-container";

import { AppState } from "@/states";
import * as SuggestionModalContainer from "./suggestion-modal-container";

export type Props = {
  state: AppState;
};

const Root = styled(Element.Component)`
  max-width: 100%;
  min-width: 0;
  max-height: 100%;
  height: 100%;
  margin: 0;
  padding: 0;

  overflow: hidden;
  background-color: ${props => props.theme.colors.base03};

  display: grid;
  grid-template-rows: 1fr 10% auto;
`;

export const Component: React.FC<Props> = ({ state }) => {
  const { fileList, notification, logEntry, taskInteraction, history, finder, completer } = state;

  return (
    <Root>
      <FileListContainer.Component state={fileList} />
      <LogViewerContainer.Component state={logEntry} />
      <NotificationContainer.Component state={notification} />
      <SuggestionModalContainer.Component state={taskInteraction} />
      <HistorySelectorContainer.Component state={history} />
      <FinderContainer.Component state={finder} />
      <CompleterContainer.Component state={completer} />
    </Root>
  );
};
