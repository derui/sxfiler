import * as React from "react";
import * as Element from "../element/element";
import { applyDisplayName } from "../util";

export type Props<T extends Element.Props = Element.Props, H extends HTMLElement = HTMLElement> = T &
  React.HTMLAttributes<H>;

export function createComponent<T extends Element.Props = Element.Props, H extends HTMLElement = HTMLElement>(
  context: {
    container?: React.ComponentType<T & React.HTMLAttributes<H>>;
  } = {}
): React.ComponentType<Props<T, H>> {
  const Container = context.container || Element.createComponent();

  return applyDisplayName("List", (props: Props<T, H>) => {
    return <Container role="list" {...props} />;
  });
}

export const Component = createComponent();
