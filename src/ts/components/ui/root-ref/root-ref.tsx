// RootRef defines component to pass reference of root node.
import * as React from "react";
import { ForwardedRef } from "@/components/ui/util";

export type Props = {
  rootRef: React.Ref<HTMLElement>;
  children: React.ReactElement<ForwardedRef>;
};

export const Component: React.FC<Props> = ({ rootRef, children }) => {
  return React.cloneElement(children, { forwardedRef: rootRef });
};
