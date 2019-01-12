import * as React from "react";
import classNames from "classnames";

import * as Domain from "../domain/filer";
import FileItem from "./file-item";


interface HeaderProp {
  directory: string;
  focused: boolean;
}

/**
 * component definition for header of file list
 */
const Header : React.FunctionComponent<HeaderProp> = ({directory, focused}) => {
  const className = classNames("fp-FileList_Header", {
    "fp-FileList_Header-focused": focused,
  });

  return (<header className={className}>{directory}</header>);
};

interface BodyProp {
  nodes: Domain.NodeMarker[];
  cursor: number;
  focused: boolean;
}

/**
 * component definition for body of file list
 */
class Body extends React.Component<BodyProp> {
  render() {
    const {nodes, cursor, focused} = this.props;

    const items = nodes.map((node, index) =>
      (<FileItem key={index} item={node.node} marked={node.marked}
                 selected={index === cursor && focused} />)
    );

    return (
      <ul className="fp-FileList_Content">{items}</ul>
    );
  }
}

interface Prop {
  location: string;
  nodes: Domain.NodeMarker[];
  cursor: number;
  focused: boolean;
}

const FileList : React.FunctionComponent<Prop> = (props) => {
  return (
    <div>
      <Header key="header" directory={props.location} focused={props.focused} />
      <Body key="body" nodes={props.nodes} cursor={props.cursor}
            focused={props.focused} />
    </div>)
};

export default FileList;
