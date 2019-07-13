import * as React from "react";
import { styled } from "@/components/theme";

import { ProgressNotification } from "@/domains/progress-notification";
import * as List from "@/components/ui/list";
import { Component as Item } from "@/components/project/progress-notification-item";

export type Props = {
  notifications: ProgressNotification[];
};

const StyledList = styled(List.Component)`
  ${List.style}
`;

export const Component: React.FC<Props> = ({ notifications }) => {
  const items = notifications.map(v => <Item key={v.id} body={v.body} />);
  return <StyledList>{items}</StyledList>;
};
