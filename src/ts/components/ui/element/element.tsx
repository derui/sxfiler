import * as React from "react";
import { applyDisplayName } from "../util";

export type Props<H extends HTMLElement = HTMLElement> = React.HTMLAttributes<H> & {
  className?: string;
};

// Type of component
export type Component = React.FC<Props>;

/**
 * Create element for base element
 */
export function createComponent<H extends HTMLElement = HTMLElement>(
  context: { tagName?: string } = {}
): React.ComponentType<Props<H>> {
  const { tagName = "div" } = context;

  return applyDisplayName("Element", (props: Props<H>) => {
    return React.createElement(tagName, props);
  });
}

export const Component = createComponent();
