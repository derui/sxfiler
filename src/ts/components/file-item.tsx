import classNames from "classnames";
import * as React from "react";
import * as Domain from "../domain/node";
import FileMode from "./file-item-mode";
import FileName from "./file-item-name";
import FileSize from "./file-item-size";
import FileTimestamp from "./file-item-timestamp";

interface Prop {
  item: Domain.Node;
  marked: boolean;
  selected: boolean;
}

export default class FileItem extends React.PureComponent<Prop> {
  public render() {
    const { item } = this.props;
    const className = classNames("fp-FileItem", {
      "fp-FileItem-selected": this.props.selected,
      "fp-FileItem-marked": this.props.marked,
    });

    return (
      <li className={className}>
        <FileMode
          key="mode"
          mode={item.stat.mode}
          isDirectory={item.stat.isDirectory}
          isSymlink={item.stat.isSymlink}
        />
        <FileTimestamp key="timestamp" timestamp={item.stat.mtime} />
        <FileSize key="size" size={item.stat.size} />
        <FileName key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      </li>
    );
  }
}
