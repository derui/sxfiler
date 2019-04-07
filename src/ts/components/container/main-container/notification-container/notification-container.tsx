import * as React from "react";

import * as Element from "../../../ui/element/element";
import { ContextLike } from "../../../../context";
import { State } from "../../../../states/notification";
import { createUseCase } from "../../../../usecases/notification/timeout";
import { Component as NotificationList } from "../../../project/notification-list/notification-list";
import LocatorContext from "../../../../locator";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./notification-container.module.scss");

export interface Props {
  state: State;
}

export class Component extends React.PureComponent<Props> {
  public render() {
    const { state } = this.props;
    const timeouts = state.timeouts.messages.map(v => v.id);

    return (
      <LocatorContext.Consumer>
        {({ context }) => {
          if (!context) {
            return null;
          }

          return (
            <Element.Component className={styles.root}>
              <NotificationList
                notifications={state.notifications.messages}
                timeouts={timeouts}
                onNotificationHidden={this.handleNotificationHidden(context)}
              />
            </Element.Component>
          );
        }}
      </LocatorContext.Consumer>
    );
  }

  private handleNotificationHidden = (context: ContextLike) => (id: string) => {
    context.execute(createUseCase(), {
      notificationId: id,
    });
  };
}
