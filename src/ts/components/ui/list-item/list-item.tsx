import * as React from "react";
import * as Element from "../element/element";
import { applyDisplayName } from "../util";

interface ComponentProps {
  selected?: boolean;
}

export type Props<T extends { className?: string } = Element.Props, H extends HTMLElement = HTMLElement> = T &
  React.HTMLAttributes<H> &
  ComponentProps;

export function createComponent<T extends { className?: string } = Element.Props, H extends HTMLElement = HTMLElement>(
  context: {
    container?: React.ComponentType<T & React.HTMLAttributes<H>>;
  } = {}
): React.ComponentType<Props<T, H>> {
  const Container = context.container || Element.createComponent();

  return applyDisplayName("ListItem", (props: Props<T, H>) => {
    const { selected = false } = props;
    return <Container role="listitem" aria-selected={selected} {...props} />;
  });
}

// export default component
export const Component = createComponent();
