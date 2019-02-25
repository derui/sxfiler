import * as React from "react";

import { NodeMarker } from "../../../domains/node-markers";
import List from "../../ui/list/list";
import { Element } from "../../ui/element/element";
import FileItem from "./../node-item/node-item";

// tslint:disable-next-line
const styles = require('./node-list.module.scss');

interface HeaderProp {
  className: string;
  directory: string;
  focused: boolean;
}

/**
 * component definition for header of file list
 */
const Header: React.FunctionComponent<HeaderProp> = ({ className, directory, focused }) => {
  return <Element tagName="header" className={className} data={{ focused }}>{directory}</Element>;
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
    <Element className={styles.container}>
      <Header key="header" className={styles.header} directory={props.location} focused={props.focused} />
      <List className={styles.nodeList} key="body">
        {items}
      </List>
    </Element>
  );
};

export default FileList;
