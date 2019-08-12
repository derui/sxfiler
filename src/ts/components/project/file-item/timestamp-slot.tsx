import * as React from "react";
import { styled } from "@/components/theme";

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

const Timestamp = styled.span`
  flex: 0 1 auto;
  padding: 0 ${props => props.theme.spaces.small};
  text-align: right;

  white-space: nowrap;
`;

export const Component: React.FC<Props> = prop => {
  const date = format(prop.timestamp);
  return <Timestamp>{date}</Timestamp>;
};
