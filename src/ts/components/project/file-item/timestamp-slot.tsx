import { h } from "preact";

export type Props = {
  timestamp: Date;
};

/**
 * format Date to display in item
 */
const format = function format(timestamp: Date): string {
  const year = `${timestamp.getFullYear()}`.padStart(4, "0");
  const month = `${timestamp.getMonth() + 1}`.padStart(2, "0");
  const date = `${timestamp.getDate()}`.padStart(2, "0");

  return `${year}/${month}/${date}`;
};

export const Component: preact.FunctionComponent<Props> = (prop) => {
  const date = format(prop.timestamp);
  return (
    <span class="file-item__item-timestamp" data-testid="fileItem-timestampSlot">
      {date}
    </span>
  );
};
