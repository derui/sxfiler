import classNames from "classnames";
import * as React from "react";

// tslint:disable-next-line
const styles = require("./list-item.module.scss");

interface Prop {
  classes?: string[];
  container?: string;
}

const ListItem: React.SFC<Prop> = props => {
  const { children, classes, container = "li" } = props;
  const className = classes ? classNames(classes) : styles.listItem;

  return React.createElement(container, { className }, children);
};

export default ListItem;
