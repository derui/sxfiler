import * as React from "react";

import { NodeMarker } from "../../../domains/node-markers";
import * as List from "../../ui/list/list";
import * as Element from "../../ui/element/element";
import NodeItem from "./../node-item/node-item";

// tslint:disable-next-line
const styles = require('./node-list.module.scss');

interface HeaderProp {
  className: string;
  directory: string;
  focused: boolean;
}

const HeaderElement = Element.createComponent({ tagName: "header" });

/**
 * component definition for header of file list
 */
const Header: React.FunctionComponent<HeaderProp> = ({ className, directory, focused }) => {
  return (<HeaderElement className={className} data-focused={focused}>{directory}</HeaderElement >);
};

interface Prop {
  location: string;
  nodes: NodeMarker[];
  cursor: number;
  focused: boolean;
}

const ListElement = List.createComponent();

const FileList: React.FunctionComponent<Prop> = props => {
  const { nodes, cursor, focused } = props;
  const items = nodes.map((node, index) => (
    <NodeItem key={index} item={node.node} marked={node.marked} selected={index === cursor && focused} />
  ));

  return (
    <Element className={styles.container}>
      <Header key="header" className={styles.header} directory={props.location} focused={props.focused} />
      <ListElement className={styles.nodeList} key="body">
        {items}
      </ListElement>
    </Element>
  );
};

export default FileList;
