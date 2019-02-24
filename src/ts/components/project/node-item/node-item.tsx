import classNames from "classnames";
import * as React from "react";
import * as Domain from "../../../domains/node";
import ListItem from "../../ui/list-item/list-item";

import Mode from "./mode-slot";
import Name from "./name-slot";
import Size from "./size-slot";
import Timestamp from "./timestamp-slot";

// tslint:disable-next-line
const styles = require("./node-item.module.scss");

interface Prop {
  item: Domain.Node;
  marked: boolean;
  selected: boolean;
}

export default class NodeItem extends React.PureComponent<Prop> {
  public render() {
    const { item } = this.props;
    const className = classNames(styles.root, {
      [styles.selected]: this.props.selected,
      [styles.marked]: this.props.marked,
    });

    return (
      <ListItem classes={[className]}>
        <Mode key="mode" mode={item.stat.mode} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
        <Timestamp key="timestamp" timestamp={item.stat.mtime} />
        <Size key="size" size={item.stat.sizeAsBigInt} />
        <Name key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      </ListItem>
    );
  }
}
