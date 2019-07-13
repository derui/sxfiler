import * as React from "react";

import { styled } from "@/components/theme";

import * as Element from "@/components/ui/element/element";
import { State } from "@/states/notification";
import { Component as NotificationList } from "@/components/project/progress-notification-list";
import LocatorContext from "@/locator";

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
    <LocatorContext.Consumer>
      {({ context }) => {
        if (!context) {
          return null;
        }

        return (
          <Root>
            <NotificationList notifications={state.progresses.notifications} />
          </Root>
        );
      }}
    </LocatorContext.Consumer>
  );
};
