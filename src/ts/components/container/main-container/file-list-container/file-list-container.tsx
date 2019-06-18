import * as React from "react";
import { Filer } from "../../../../domains/filer";
import { Side, State } from "../../../../states/file-list";
import * as NodeList from "../../../project/node-list/node-list";
import * as Element from "../../../ui/element/element";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles: ClassNames = require("./file-list-container.module.scss");

interface ClassNames {
  root: string;
  separator: string;
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
  if (!props.state.left || !props.state.right) {
    return null;
  }

  const leftFiler = createFiler(Side.Left, props.state.currentSide, props.state.left);
  const rightFiler = createFiler(Side.Right, props.state.currentSide, props.state.right);

  return (
    <Element.Component className={styles.root}>
      {leftFiler}
      <div className={styles.separator} />
      {rightFiler}
    </Element.Component>
  );
};
