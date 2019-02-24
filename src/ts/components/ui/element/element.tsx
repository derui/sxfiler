import * as React from "react";

export interface ElementProp extends React.HTMLAttributes<HTMLElement> {
  classes?: { [key: string]: string };
  tagName?: string;
  data?: { [key: string]: any };
  aria?: { [key: string]: any };
}

export const Element: React.FC<ElementProp> = props => {
  const {
    className,
    children,
    // classes omit in this element.
    classes = {},
    tagName = "div",
    data = {},
    aria = {},
    ...rest
  } = props;

  const dataSet = Object.entries(data).map(([key, value]) => [`data-${key}`, value])
    .reduce((obj, [key, value]) => ({ ...obj, [key]: value }), {});

  const ariaSet = Object.entries(aria).map(([key, value]) => [`aria-${key}`, value])
    .reduce((obj, [key, value]) => ({ ...obj, [key]: value }), {});

  return React.createElement(tagName, { ...rest, ...dataSet, ...ariaSet, className }, children);
};
