import * as React from "react";
import {State, Side} from "../types/store-state/file-list";
import * as Filer from "../domain/filer";
import FileList from "../components/file-list";
import "./workspace.css";

interface Prop {
  state: State;
}

/* create filer from state and key */
function createFiler(key:string, filer : Filer.Filer) {
  return (
    <FileList key={key} nodes={filer.nodes} cursor={filer.currentCursorIndex} location={filer.location} />
  );
}

// Stateless container to render filer
const FileListContainer : React.SFC<Prop> = props => {
  // can not render anything if filer is not initialized
  if (!props.state.initialized) {
    return null;
  }

  const filers = [
    createFiler(Side.Left, props.state.left!),
    createFiler(Side.Right, props.state.right!)
  ];

    return (
      <div className="fp-FileListContainer">
        {filers}
      </div>
    );
}

export default FileListContainer;
