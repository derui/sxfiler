import * as React from "react";

import * as Element from "../../../ui/element/element";
import { State } from "../../../../states/log-entry";
import { Component as LogViewer } from "../../../project/log-viewer/log-viewer";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./log-viewer-container.module.scss");

export interface Props {
  state: State;
}

export const Component: React.FC<Props> = ({ state }) => {
  return (
    <Element.Component className={styles.root}>
      <LogViewer entries={Array.from(state.entries.values())} hidden={false} />
    </Element.Component>
  );
};
