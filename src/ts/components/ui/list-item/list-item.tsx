import * as React from "react";
import { Element, ElementProp } from "../element/element";

interface Prop extends ElementProp {
  selected?: boolean;
}

const ListItem: React.SFC<Prop> = props => {
  const { children, selected = false, aria, ...rest } = props;

  return (<Element role="listitem" aria={{ ...aria, selected }} {...rest}>{children}</Element>)
};

export default ListItem;
