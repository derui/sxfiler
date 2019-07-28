import * as React from "react";

export type ElementProps = {
  className?: string;
};

export type Props<H extends HTMLElement = HTMLElement> = React.HTMLAttributes<H> & ElementProps;

// Type of component
export type ComponentType = React.FC<Props>;

/**
 * Create element for base element
 */
export function createComponent<H extends HTMLElement = HTMLElement>(
  context: { tagName?: string } = {}
): React.ComponentType<Props<H> & React.RefAttributes<H>> {
  const { tagName = "div" } = context;

  const render = (props: Props<H>, ref: React.Ref<H>) => {
    return React.createElement(tagName, { ...props, ref });
  };
  /* render.displayName = "Element"; */
  return React.forwardRef(render);
}

export const Component = createComponent();
