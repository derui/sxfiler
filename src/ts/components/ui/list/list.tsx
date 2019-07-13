import * as React from "react";
import { css } from "@/components/theme";
import * as Element from "@/components/ui/element/element";
import { applyDisplayName } from "@/components/ui/util";

export type Props<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement> = T &
  React.HTMLAttributes<H>;

export const style = css`
  display: flex;
  flex-direction: column;

  flex: 1 1 auto;
`;

export function createComponent<T extends Element.Props<H> = Element.Props, H extends HTMLElement = HTMLElement>(
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
