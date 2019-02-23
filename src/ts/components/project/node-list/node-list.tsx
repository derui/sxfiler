import classNames from "classnames";
import * as React from "react";

import { NodeMarker } from "../../domains/node-markers";
import List from "../ui/list/list";
import FileItem from "./file-item";

// tslint:disable-next-line
const styles = require('./file-list.module.scss');

interface HeaderProp {
  directory: string;
  focused: boolean;
}

/**
 * component definition for header of file list
 */
const Header: React.FunctionComponent<HeaderProp> = ({ directory, focused }) => {
  const className = classNames("fp-FileList_Header", {
    "fp-FileList_Header-focused": focused,
  });

  return <header className={className}>{directory}</header>;
};

interface Prop {
  location: string;
  nodes: NodeMarker[];
  cursor: number;
  focused: boolean;
}

const FileList: React.FunctionComponent<Prop> = props => {
  const { nodes, cursor, focused } = props;
  const items = nodes.map((node, index) => (
    <FileItem key={index} item={node.node} marked={node.marked} selected={index === cursor && focused} />
  ));

  return (
    <div>
      <Header key="header" directory={props.location} focused={props.focused} />
      <List classes={[styles.list]} key="body">
        {items}{" "}
      </List>
    </div>
  );
};

export default FileList;
