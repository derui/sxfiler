import * as React from "react";
import * as MainContainer from "./components/container/main-container/main-container";
import { AppState } from "./states";

export type Props = {
  state: AppState;
};

export class Component extends React.Component<Props> {
  public render(): React.ReactElement {
    return <MainContainer.Component state={this.props.state} />;
  }
}
