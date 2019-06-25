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

// push new entry to state
export const pushEntry = (state: State, obj: MessageNotification): State => {
  const newEntry = new Map(state.entries);

  newEntry.set(obj.id, obj);
  return {
    entries: newEntry,
  };
};
