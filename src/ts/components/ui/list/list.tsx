import classNames from "classnames";
import * as React from "react";

// tslint:disable-next-line
const styles = require("./list.module.scss");

interface Prop extends React.HTMLAttributes<HTMLElement> {
  classes?: string[];
  container?: string;
}

const FileList: React.FC<Prop> = props => {
  const { classes, container = "ul", children, ...rest } = props;
  const className = classes ? classNames(classes) : styles.list;

  return React.createElement(container, { ...rest, className }, children);
};

export default FileList;
