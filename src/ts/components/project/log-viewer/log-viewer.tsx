import * as React from "react";
import { styled } from "@/components/theme";

import * as ListItem from "@/components/ui/list-item";
import * as List from "@/components/ui/list";
import { MessageNotification, Level } from "@/domains/message-notification";

export type Props = {
  entries: MessageNotification[];
  hidden: boolean;
};

const Root = styled(List.Component)`
  ${List.style}

  background-color: ${props => props.theme.colors.base03};

  color: ${props => props.theme.colors.base2};

  height: 100%;
  font-size: 1rem;
  overflow-x: auto;
  overflow-y: scroll;

  font-family: monospace;
`;

const LogItem = styled(ListItem.Component)`
  display: flex;
  flex-direction: row;
  padding: ${props => props.theme.spaces.nano} 0;
`;

const ItemLevel = styled.span`
  flex: 0 0 4rem;
  white-space: pre;
  margin-right: 1rem;

  &[data-level="info"] {
    color: ${props => props.theme.colors.blue};
  }

  &[data-level="warning"] {
    color: ${props => props.theme.colors.orange};
  }

  &[data-level="error"] {
    color: ${props => props.theme.colors.red};
  }
`;

const ItemMessage = styled.span`
  flex: 0 0 auto;
`;

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
    <LogItem key={entry.id}>
      <ItemLevel data-level={level}>[{level.toUpperCase()}]</ItemLevel>
      <ItemMessage>{entry.body}</ItemMessage>
    </LogItem>
  );
};

export const Component: React.FC<Props> = ({ entries, hidden }) => {
  let components: React.ReactElement[] = [];
  if (!hidden) {
    components = entries.map(makeItem);
  }

  return <Root aria-hidden={hidden}>{components}</Root>;
};
