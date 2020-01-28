import { h } from "preact";
import { Component as LogViewer } from "@/components/project/log-viewer";

export type Props = {};

export const Component: preact.FunctionComponent<Props> = () => {
  return (
    <div class="log-viewer-container__root" data-testid="logViewerContainer">
      <LogViewer entries={[]} hidden={false} />
    </div>
  );
};
