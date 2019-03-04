import * as React from "react";
import { Filer } from "../../../domains/filer";
import { Side, State } from "../../../types/store-state/file-list";
import * as NodeList from "../../project/node-list/node-list";

/* eslint-disable @typescript-eslint/no-var-requires */
const styles: ClassNames = require("./file-list-container.module.scss");
/* eslint-enable @typescript-eslint/no-var-requires */

interface ClassNames {
  root: string;
}

export interface Props {
  state: State;
}

/* create filer from state and key */
function createFiler(key: string, currentSide: Side, filer?: Filer): NodeList.ElementType | null {
  const focused = key === currentSide;

  if (!filer) {
    return null;
  }

  return (
    <NodeList.Component
      key={key}
      nodes={filer.nodes}
      cursor={filer.currentCursorIndex}
      location={filer.location}
      focused={focused}
    />
  );
}

export type ElementType = React.ReactElement<Props, React.FC<Props>>;

// Stateless container to render filer
export const Component: React.FC<Props> = (props): ElementType | null => {
  // can not render anything if filer is not initialized
  if (!props.state.initialized) {
    return null;
  }

  const filers = [
    createFiler(Side.Left, props.state.currentSide, props.state.left),
    createFiler(Side.Right, props.state.currentSide, props.state.right),
  ];

  return <div className={styles.root}>{filers}</div>;
};
