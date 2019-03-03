import * as React from "react";
import { ContextLike } from "../../../context";
import { State } from "../../../types/store-state/notification";
import TimeoutUseCase from "../../../usecases/notification/timeout";
import * as NotificationList from "../../project/notification-list/notification-list";

// tslint:disable-next-line
const styles = require("./notification-container.module.scss");

export interface Props {
  context: ContextLike;
  state: State;
}

export class Component extends React.PureComponent<Props> {
  public render() {
    const { state } = this.props;
    const timeouts = state.timeouts.messages.map(v => v.id);

    return (
      <div className={styles.root}>
        <NotificationList.Component
          notifications={state.notifications.messages}
          timeouts={timeouts}
          onNotificationHidden={this.handleNotificationHidden}
        />
      </div>
    );
  }
  private handleNotificationHidden = (id: string) => {
    this.props.context.execute(new TimeoutUseCase(), {
      notificationId: id,
    });
  };
}
