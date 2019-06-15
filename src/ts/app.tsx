import * as React from "react";
import * as MainContainer from "./components/container/main-container/main-container";
import { AppState } from "./states";

export type Props = {
  state: AppState;
};

export const Component: React.FC<Props> = ({ state }) => <MainContainer.Component state={state} />;
