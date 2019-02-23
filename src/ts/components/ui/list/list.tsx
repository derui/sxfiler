import classNames from "classnames";
import * as React from "react";
const styles = require("./list.module.css");

interface Prop {
  classes?: string[];
  container?: string;
}

const FileList: React.SFC<Prop> = props => {
  const {classes, container = "ul", children} = props;
  const className = classes ? classNames(classes) : styles.list;

  return React.createElement(container, {className}, children);
};

export default FileList;
