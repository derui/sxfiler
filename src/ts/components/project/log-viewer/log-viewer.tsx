import * as React from "react";

import * as ListItem from "../../ui/list-item/list-item";
import * as List from "../../ui/list/list";
import { MessageNotification, Level } from "../../../domains/message-notification";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles: ClassNames = require("./log-viewer.module.scss");

export interface ClassNames {
  root?: string;
  item?: string;
  itemLevel?: string;
  itemMessage?: string;
}

export type Props = {
  entries: MessageNotification[];
  hidden: boolean;
};

/**
 * make item for a log entry
 */
const makeItem = (entry: MessageNotification) => {
  let level = "info";
  switch (entry.level) {
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

  return (
    <ListItem.Component key={entry.id} className={styles.item}>
      <span className={styles.itemLevel} data-level={level}>
        [{level.toUpperCase()}]
      </span>
      <span className={styles.itemMessage}>{entry.body}</span>
    </ListItem.Component>
  );
};

export const Component: React.FC<Props> = ({ entries, hidden }) => {
  let components: React.ReactElement[] = [];
  if (!hidden) {
    components = entries.map(makeItem);
  }

  return (
    <List.Component className={styles.root} aria-hidden={hidden}>
      {components}
    </List.Component>
  );
};
