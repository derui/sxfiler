import * as React from "react";
import { StoreState } from "../../types/store-state";
import FileListContainer from "./file-list-container";

interface Prop {
  state: StoreState;
}

export default class Workspace extends React.Component<Prop> {
  private layoutRef: React.RefObject<any>;

  public constructor(props: Prop) {
    super(props);
    this.layoutRef = React.createRef();
  }

  public componentDidMount() {
    if (this.layoutRef.current) {
      this.layoutRef.current.focus();
    }
  }

  public componentDidUpdate() {
    if (this.layoutRef.current) {
      this.layoutRef.current.focus();
    }
  }

  public render() {
    return (
      <div key="layout" className="fp-Workspace" tabIndex={0} ref={this.layoutRef}>
        <FileListContainer key="filer" state={this.props.state.fileList} />
      </div>
    );
  }
}
