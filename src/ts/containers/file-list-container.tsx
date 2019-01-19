import * as React from "react";
import FileList from "../components/file-list";
import * as Filer from "../domain/filer";
import { Side, State } from "../types/store-state/file-list";
import "./workspace.css";

interface Prop {
  state: State;
}

/* create filer from state and key */
function createFiler(key: string, currentSide: Side, filer: Filer.Filer) {
  const focused = key === currentSide;
  return (
    <FileList
      key={key}
      nodes={filer.nodes}
      cursor={filer.currentCursorIndex}
      location={filer.location}
      focused={focused}
    />
  );
}

// Stateless container to render filer
const FileListContainer: React.FC<Prop> = props => {
  // can not render anything if filer is not initialized
  if (!props.state.initialized) {
    return null;
  }

  const filers = [
    createFiler(Side.Left, props.state.currentSide, props.state.left!),
    createFiler(Side.Right, props.state.currentSide, props.state.right!),
  ];

  return <div className="fp-FileListContainer">{filers}</div>;
};

export default FileListContainer;
