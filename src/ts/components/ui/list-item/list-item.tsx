import classNames from "classnames";
import * as React from "react";

// tslint:disable-next-line
const styles = require("./list-item.module.scss");

interface Prop extends React.HTMLAttributes<HTMLElement> {
  classes?: string[];
  container?: string;
}

const ListItem: React.SFC<Prop> = props => {
  const { children, classes, container = "li", ...rest } = props;
  const className = classes ? classNames(classes) : styles.listItem;

  return React.createElement(container, { ...rest, className }, children);
};

export default ListItem;
