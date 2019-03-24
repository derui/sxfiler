import * as React from "react";
import * as MainContainer from "./components/container/main-container/main-container";
import { StoreState } from "./types/store-state";

export type Props = {
  state: StoreState;
};

export class Component extends React.Component<Props> {
  public render(): React.ReactElement {
    return <MainContainer.Component state={this.props.state} />;
  }
}
