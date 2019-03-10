// RootRef defines component to pass reference of root node.
import * as React from "react";
import * as Element from "../element/element";

export interface Props {
  rootRef: React.Ref<HTMLElement>;
  children: React.ReactElement<Element.Props>;
}

export const Component: React.FC<Props> = ({ rootRef, children }) => {
  return React.cloneElement(children, { forwardedRef: rootRef });
};
