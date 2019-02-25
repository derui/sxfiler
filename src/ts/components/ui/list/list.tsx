import * as React from "react";
import * as Element from "../element/element";

export type Props<
  T extends { className?: string } = Element.Props,
  H extends HTMLElement = HTMLElement>
  = T & React.HTMLAttributes<H>

export type Component = React.FC<Props>;

export function createComponent<
  T extends { className?: string } = Element.Props,
  H extends HTMLElement = HTMLElement
>(context: {
  container?: React.ComponentType<T & React.HTMLAttributes<H>>;
} = {}
): React.ComponentType<Props<T, H>> {
  const Container = context.container || Element.createComponent();

  return (props: Props<T, H>) => {

    return (<Container role="list" {...props} />);
  };
}
