import * as React from "react";
import { css } from "@/components/theme";
import * as Element from "@/components/ui/element/element";

export type Props<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement> = T & {
  selected?: boolean;
};

export const style = css`
  display: flex;
  flex-direction: row;

  // show original item forcely
  flex: 0 0 auto;

  list-style: none;
`;

export const createComponent = function createComponent<
  T extends Element.Props<H> = Element.Props,
  H extends HTMLElement = HTMLElement
>(
  context: {
    container?: React.ComponentType<T & React.RefAttributes<H>>;
  } = {}
) {
  const Container = context.container || Element.Component;

  const render = ({ selected = false, ...rest }: Props<T, H>, ref: React.Ref<H>) => {
    // TODO: need cast?
    return <Container role="listitem" ref={ref} aria-selected={selected} {...(rest as T)} />;
  };
  render.displayName = "ListItem";

  return React.forwardRef(render);
};

export const Component = createComponent();
