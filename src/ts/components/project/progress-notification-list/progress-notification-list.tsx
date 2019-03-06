import * as React from "react";

import { Notification } from "../../../domains/notification";
import * as List from "../../ui/list/list";
import { Component as Item } from "../progress-notification-item/progress-notification-item";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./progress-notification-list.module.scss");

export interface Props {
  notifications: Notification[];
}

export const Component: React.FC<Props> = ({ notifications }) => {
  const items = notifications.map(v => <Item key={v.id} body={v.getProgressBody()} />);
  return <List.Component className={styles.root}>{items}</List.Component>;
};
