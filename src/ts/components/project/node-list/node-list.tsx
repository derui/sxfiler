import * as React from "react";

import { Node } from "../../../domains/node";
import * as Element from "../../ui/element/element";
import * as List from "../../ui/list/list";
import NodeItem from "./../node-item/node-item";

// tslint:disable-next-line
const styles: ClassNames = require('./node-list.module.scss');

export interface ClassNames {
  root?: string;
  header?: string;
  list?: string;
  empty?: string;
}

interface HeaderProp {
  className?: string;
  directory: string;
  focused: boolean;
}

const HeaderElement = Element.createComponent({ tagName: "header" });

/**
 * component definition for header of file list
 */
const Header: React.FunctionComponent<HeaderProp> = ({ className, directory, focused }) => {
  return (
    <HeaderElement className={className} data-focused={focused}>
      {directory}
    </HeaderElement>
  );
};

interface Prop {
  location: string;
  nodes: Node[];
  cursor: number;
  focused: boolean;
}

const FileList: React.FunctionComponent<Prop> = props => {
  const { nodes, cursor, focused } = props;
  const items = nodes.map((node, index) => <NodeItem key={index} item={node} selected={index === cursor && focused} />);

  return (
    <Element.Component className={styles.root}>
      <Header key="header" className={styles.header} directory={props.location} focused={props.focused} />
      {items.length === 0 ? (
        <div className={styles.empty} />
      ) : (
        <List.Component className={styles.list} key="body">
          {items}
        </List.Component>
      )}
    </Element.Component>
  );
};

export default FileList;
