import * as React from "react";
import { Context } from "../../../context";
import { State } from "../../../types/store-state/notification";
import TimeoutUseCase from "../../../usecases/notification/timeout";
import { TimeoutCallback } from "../../project/notification-item/notification-item";
import * as NotificationList from "../../project/notification-list/notification-list";

// tslint:disable-next-line
const styles = require("./notification-container.module.scss");

export interface Props {
  context: Context;
  state: State;
}

export class Component extends React.PureComponent<Props> {
  public render() {
    const { state } = this.props;

    return (
      <div className={styles.root}>
        <NotificationList.Conmonent
          notifications={state.notifications.items}
          onItemTimeouted={this.handleItemTimeouted}
        />
      </div>
    );
  }
  private handleItemTimeouted: TimeoutCallback = (id: string) => {
    this.props.context.execute(new TimeoutUseCase(), {
      notificationId: id,
    });
  };
}
