import * as React from "react";
import { Notification } from "../../domains/notification";
import NotificationItem, { TimeoutCallback } from "./notification-item";

interface Prop {
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
  return <NotificationItem item={notification} timeouted={timeouted} onItemTimeouted={onItemTimeouted} />;
}

const NotificationList: React.FC<Prop> = prop => {
  const { notifications, timeouts, onItemTimeouted } = prop;

  const components = notifications.map(v => toComponent(v, timeouts, onItemTimeouted));

  return <ul className="fp-NotificationList">{components}</ul>;
};

export default NotificationList;
