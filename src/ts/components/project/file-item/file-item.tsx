import * as React from "react";
import * as Domain from "../../../domains/file-item";
import * as ListItem from "../../ui/list-item/list-item";

import Mode from "./mode-slot";
import Name from "./name-slot";
import Size from "./size-slot";
import Timestamp from "./timestamp-slot";
import { ForwardedRef } from "../../ui/util";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./file-item.module.scss");

export type Props = ForwardedRef & {
  item: Domain.FileItem;
  selected: boolean;
  hidden?: boolean;
};

const Element = ListItem.createComponent();

export class Component extends React.PureComponent<Props> {
  public render() {
    const { item, selected, hidden = false, ...rest } = this.props;

    return (
      <Element
        className={styles.fileItem}
        aria-selected={selected}
        data-marked={item.marked}
        aria-hidden={hidden}
        {...rest}
      >
        <Mode key="mode" mode={item.stat.mode} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
        <Timestamp key="timestamp" timestamp={item.stat.mtime} />
        <Size key="size" size={item.stat.sizeAsBigInt()} />
        <Name key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      </Element>
    );
  }
}
