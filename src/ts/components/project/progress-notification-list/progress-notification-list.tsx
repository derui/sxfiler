import * as React from "react";

import { ProgressNotification } from "../../../domains/progress-notification";
import * as List from "../../ui/list/list";
import { Component as Item } from "../progress-notification-item/progress-notification-item";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./progress-notification-list.module.scss");

export interface Props {
  notifications: ProgressNotification[];
}

export const Component: React.FC<Props> = ({ notifications }) => {
  const items = notifications.map(v => <Item key={v.id} body={v.body} />);
  return <List.Component className={styles.root}>{items}</List.Component>;
};
