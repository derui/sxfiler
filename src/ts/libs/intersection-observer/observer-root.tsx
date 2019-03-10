import * as React from "react";
import * as observer from "./observer-manager";

export interface Props {
  children: (rootId: observer.RootId) => React.ReactNode;
}

interface State {
  rootId?: observer.RootId;
}

export class Component extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {};
  }

  componentWillMount() {
    this.setState({
      rootId: observer.createRoot(),
    });
  }

  componentWillUnmount() {
    const { rootId } = this.state;

    if (rootId) {
      observer.deleteRoot(rootId);
    }
  }

  render() {
    if (!this.state.rootId) {
      return null;
    }
    return this.props.children(this.state.rootId);
  }
}
