import * as React from "react";

interface Prop {
  timestamp: Date;
}

/**
 * format Date to display in item
 */
function format(timestamp: Date): string {
  const year = `${timestamp.getFullYear()}`.padStart(4, "0");
  const month = `${timestamp.getMonth() + 1}`.padStart(2, "0");
  const date = `${timestamp.getDate()}`.padStart(2, "0");

  return `${year}/${month}/${date}`;
}

const NodeTimestamp: React.FC<Prop> = prop => {
  const date = format(prop.timestamp);
  return <span className="fp-FileItem_Timestamp">{date}</span>;
};

export default NodeTimestamp;
