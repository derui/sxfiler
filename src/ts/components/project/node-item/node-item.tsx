import * as React from "react";
import * as Domain from "../../../domains/node";
import * as ListItem from "../../ui/list-item/list-item";

import Mode from "./mode-slot";
import Name from "./name-slot";
import Size from "./size-slot";
import Timestamp from "./timestamp-slot";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./node-item.module.scss");

export interface Props {
  item: Domain.Node;
  selected: boolean;
}

const Element = ListItem.createComponent();

export class Component extends React.PureComponent<Props> {
  public render() {
    const { item, selected } = this.props;

    return (
      <Element className={styles.nodeItem} aria-selected={selected} data-marked={item.marked}>
        <Mode key="mode" mode={item.stat.mode} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
        <Timestamp key="timestamp" timestamp={item.stat.mtime} />
        <Size key="size" size={item.stat.sizeAsBigInt} />
        <Name key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      </Element>
    );
  }
}
