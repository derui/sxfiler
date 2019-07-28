import * as React from "react";
import { css } from "@/components/theme";
import * as Element from "@/components/ui/element/element";

export type Props<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement> = T &
  React.HTMLAttributes<H>;

export const style = css`
  display: flex;
  flex-direction: column;

  flex: 1 1 auto;
`;

export function createComponent<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement>(
  context: {
    container?: React.ComponentType<T & React.RefAttributes<H>>;
  } = {}
) {
  const Container = context.container || Element.createComponent();

  const render = (props: Props<T, H>, ref: React.Ref<H>) => {
    return <Container role="list" ref={ref} {...props} />;
  };
  render.displayName = "List";

  return React.forwardRef(render);
}

export const Component = createComponent();
