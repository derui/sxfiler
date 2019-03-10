import * as React from "react";
import * as Element from "../element/element";
import { applyDisplayName } from "../util";

export type Props<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement> = T &
  React.HTMLAttributes<H> & {
    selected?: boolean;
  };

export function createComponent<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement>(
  context: {
    container?: React.ComponentType<T & React.HTMLAttributes<H>>;
  } = {}
): React.ComponentType<Props<T, H>> {
  const Container = context.container || Element.createComponent();

  return applyDisplayName("ListItem", ({ selected = false, ...rest }: Props<T, H>) => {
    // TODO: need cast?
    return <Container role="listitem" aria-selected={selected} {...rest as T} />;
  });
}

// export default component
export const Component = createComponent();
