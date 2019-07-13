import * as React from "react";
import { styled } from "@/components/theme";

import * as Element from "@/components/ui/element";
import { State } from "@/states/log-entry";
import { Component as LogViewer } from "@/components/project/log-viewer";

export type Props = {
  state: State;
};

const Root = styled(Element.Component)`
  position: relative;

  height: 100%;
  width: 100%;
  max-width: 100%;
  border-top: 1px solid ${props => props.theme.colors.base2};
`;

export const Component: React.FC<Props> = ({ state }) => {
  return (
    <Root>
      <LogViewer entries={Array.from(state.entries.values())} hidden={false} />
    </Root>
  );
};
