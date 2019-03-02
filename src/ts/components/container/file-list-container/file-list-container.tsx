import * as React from "react";
import { Filer } from "../../../domains/filer";
import { Side, State } from "../../../types/store-state/file-list";
import NodeList from "../../project/node-list/node-list";

// tslint:disable-next-line
const styles: ClassNames = require("./file-list-container.module.scss");

interface ClassNames {
  root: string;
}

export interface Props {
  state: State;
}

/* create filer from state and key */
function createFiler(key: string, currentSide: Side, filer: Filer) {
  const focused = key === currentSide;
  return (
    <NodeList
      key={key}
      nodes={filer.nodes}
      cursor={filer.currentCursorIndex}
      location={filer.location}
      focused={focused}
    />
  );
}

// Stateless container to render filer
export const Component: React.FC<Props> = props => {
  // can not render anything if filer is not initialized
  if (!props.state.initialized) {
    return null;
  }

  const filers = [
    createFiler(Side.Left, props.state.currentSide, props.state.left!),
    createFiler(Side.Right, props.state.currentSide, props.state.right!),
  ];

  return <div className={styles.root}>{filers}</div>;
};

export default Component;
