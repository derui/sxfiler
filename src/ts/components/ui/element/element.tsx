import * as React from "react";
import { applyDisplayName } from "../util";

export type ElementProps<H> = {
  forwardedRef?: React.Ref<H>;
  className?: string;
};

export type Props<H extends HTMLElement = HTMLElement> = React.HTMLAttributes<H> & ElementProps<H>;

// Type of component
export type ComponentType = React.FC<Props>;

/**
 * Create element for base element
 */
export function createComponent<H extends HTMLElement = HTMLElement>(
  context: { tagName?: string } = {}
): React.ComponentType<Props<H> & React.RefAttributes<H>> {
  const { tagName = "div" } = context;

  return applyDisplayName("Element", ({ forwardedRef, ...props }: Props<H>) => {
    return React.createElement(tagName, { ...props, ref: forwardedRef });
  });
}

export const Component = createComponent();
