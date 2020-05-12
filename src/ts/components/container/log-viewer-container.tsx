import { h } from "preact";
import { Component as LogViewer } from "@/components/project/log-viewer";
import { State } from "@/modules";

export type Props = {
  state: State;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  return (
    <div class="log-viewer-container__root" data-testid="logViewerContainer">
      <LogViewer entries={state.logEvent.events} hidden={false} />
    </div>
  );
};
