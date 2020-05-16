import { ActionTypes } from "./types";
import { Actions } from "./actions";

type LogEvent = {
  timestamp: Date;
};

type MoveItemEvent = LogEvent & {
  kind: "MOVE_EVENT";
  source: string;
  destination: string;
};

type CopyItemEvent = LogEvent & {
  kind: "COPY_EVENT";
  source: string;
  destination: string;
};

type DeleteItemEvent = LogEvent & {
  kind: "DELETE_EVENT";
  fullPath: string;
};

type KeymapReloadEvent = LogEvent & {
  kind: "KEYMAP_RELOAD_EVENT";
};

export type LogEvents = MoveItemEvent | CopyItemEvent | DeleteItemEvent | KeymapReloadEvent;

export namespace LogEventCreators {
  export const createMoveItem = (timestamp: Date, source: string, destination: string): MoveItemEvent => ({
    kind: "MOVE_EVENT",
    timestamp,
    source,
    destination,
  });

  export const createCopyItem = (timestamp: Date, source: string, destination: string): CopyItemEvent => ({
    kind: "COPY_EVENT",
    timestamp,
    source,
    destination,
  });

  export const createDeleteItem = (timestamp: Date, fullPath: string): DeleteItemEvent => ({
    kind: "DELETE_EVENT",
    timestamp,
    fullPath,
  });

  export const createKeymapReload = (timestamp: Date): KeymapReloadEvent => ({
    kind: "KEYMAP_RELOAD_EVENT",
    timestamp,
  });
}

// state of type. Please redefine to what you want.
export type State = {
  events: LogEvents[];
};

export const emptyState: State = { events: [] };

const addEvents = (state: State, events: LogEvents[]) => {
  return Object.freeze({ ...state, events: state.events.concat(events) });
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.SEND:
      return addEvents(state, action.payload);
    default:
      return state;
  }
};
