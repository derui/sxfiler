import * as React from "react";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./scroll-bar.module.scss");

export interface Props {
  start: number;
  windowSize: number;
}

export const Component: React.FC<Props> = ({ start, windowSize }) => {
  const top = `${start * 100.0}%`;
  const height = `${windowSize * 100.0}%`;

  return (
    <div className={styles.root}>
      <div className={styles.bar} style={{ top, height }} />
    </div>
  );
};
