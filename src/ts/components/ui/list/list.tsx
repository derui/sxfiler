import * as React from "react";
import { Element, ElementProp } from "../element/element";

interface Prop extends ElementProp {
}

const FileList: React.FC<Prop> = props => {
  const { className, children, ...rest } = props;

  return (<Element className={className} role="list" {...rest}>{children}</Element>);
};

export default FileList;
