import * as React from "react";
import { Notification } from "../../../domains/notification";
import * as List from "../../ui/list/list";
import NotificationItem, { TimeoutCallback } from "../notification-item/notification-item";

// tslint:disable-next-line
const styles: ClassNames = require("./notification-list.module.scss");

type ClassNames = {
  root: string;
}

interface Props {
  // notifications given from server
  notifications: Notification[];
  // list of timeouted notification id
  timeouts: string[];
  // callback to timeout item
  onItemTimeouted: TimeoutCallback;
}

// make item
function toComponent(notification: Notification, timeouts: string[], onItemTimeouted: TimeoutCallback) {
  const timeouted = timeouts.find(v => v === notification.id) !== undefined;
  return <NotificationItem key={notification.id} item={notification} timeouted={timeouted} onItemTimeouted={onItemTimeouted} />;
}

export const NotificationList: React.FC<Props> = props => {
  const { notifications, timeouts, onItemTimeouted } = props;

  const components = notifications.map(v => toComponent(v, timeouts, onItemTimeouted));

  return <List.Component className={styles.root}>{components}</List.Component>;
};

export default NotificationList;
