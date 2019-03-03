import * as React from "react";
import { Notification } from "../../../domains/notification";
import * as List from "../../ui/list/list";
import * as NotificationItem from "../notification-item/notification-item";

// tslint:disable-next-line
const styles: ClassNames = require("./notification-list.module.scss");

interface ClassNames {
  root: string;
}

export interface Props {
  // notifications given from server
  notifications: Notification[];
  onNotificationHidden: (id: string) => void;

  // notification ids that are timeouted and not yet removed.
  timeouts: string[];
}

// make item
function toComponent(notification: Notification, timeouts: string[], handle: (id: string) => void) {
  const C = NotificationItem.Component;
  const timeouted = timeouts.includes(notification.id);
  const handleExited = () => handle(notification.id);
  return (
    <C
      key={notification.id}
      body={notification.getMessageBody()}
      level={notification.level}
      onExited={handleExited}
      timeouted={timeouted}
    />
  );
}

export const Component: React.FC<Props> = props => {
  const { notifications, timeouts } = props;

  const components = notifications.map(v => toComponent(v, timeouts, props.onNotificationHidden));

  return <List.Component className={styles.root}>{components}</List.Component>;
};
