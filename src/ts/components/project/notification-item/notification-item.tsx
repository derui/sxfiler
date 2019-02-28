import * as React from "react";
import { CSSTransition } from "react-transition-group";
import { Level, Notification } from "../../../domains/notification";

import * as ListItem from "../../ui/list-item/list-item";

// tslint:disable-next-line
const styles: ClassNames = require("./notification-item.module.scss");

interface ClassNames {
  root: string;
  animation: string;
}

export type TimeoutCallback = (id: string) => void;

export interface Props {
  item: Notification;
  timeouted: boolean;
  onItemTimeouted: TimeoutCallback;
}

interface State {
  mounted: boolean;
}

export default class NotificationItem extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
  }

  public render() {
    const { item } = this.props;

    const renderer = () => {
      let level = "";
      switch (item.level) {
        case Level.Info:
          level = "info";
          break;
        case Level.Warning:
          level = "warning";
          break;
        case Level.Error:
          level = "error";
          break;
      }

      switch (item.body.kind) {
        case "oneshot":
          return (
            <ListItem.Component className={styles.root} data-level={level}>
              {item.body.message}
            </ListItem.Component>
          );
        case "progress":
          return null;
      }
    };

    return (
      <CSSTransition
        in={!this.props.timeouted}
        onExited={this.handleItemTimeouted}
        timeout={200}
        unmountOnExit={true}
        classNames={styles.animation}
      >
        {renderer}
      </CSSTransition>
    );
  }

  private handleItemTimeouted = () => {
    this.props.onItemTimeouted(this.props.item.id);
  };
}
