import { MessageNotification } from "../domains/message-notification";

// define state to store log entries

export type State = {
  entries: Map<string, MessageNotification>;
};

// get new state
export const empty = (): State => {
  return {
    entries: new Map(),
  };
};
