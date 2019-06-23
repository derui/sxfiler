import * as React from "react";

import * as Element from "../../../ui/element/element";
import { State } from "../../../../states/notification";
import { Component as NotificationList } from "../../../project/progress-notification-list/progress-notification-list";
import LocatorContext from "../../../../locator";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./notification-container.module.scss");

export interface Props {
  state: State;
}

export const Component: React.FC<Props> = ({ state }) => {
  return (
    <LocatorContext.Consumer>
      {({ context }) => {
        if (!context) {
          return null;
        }

        return (
          <Element.Component className={styles.root}>
            <NotificationList notifications={state.progresses.notifications} />
          </Element.Component>
        );
      }}
    </LocatorContext.Consumer>
  );
};
