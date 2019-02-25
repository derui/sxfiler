import * as React from "react";

export type Props<H extends HTMLElement = HTMLElement> = React.HTMLAttributes<H> & {
  className?: string;
}

// Type of component
export type Component = React.FC<Props>

/**
   Create element for base element
 */
export function createComponent(context: { tagName?: string } = {}): React.ComponentType<Props> {
  const { tagName = "div" } = context;

  return ({ className, ...props }: Props) => {
    return React.createElement(tagName, { className, ...props });
  }
};
