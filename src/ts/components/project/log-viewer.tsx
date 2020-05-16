import { h } from "preact";
import { LogEvents } from "@/modules/log-event";

export type Props = {
  entries: LogEvents[];
  hidden: boolean;
};

const logEventToEntry = (event: LogEvents) => {
  const timestamp = event.timestamp.toISOString();
  switch (event.kind) {
    case "COPY_EVENT":
      return `[${timestamp}] Copied: ${event.source} -> ${event.destination}`;
    case "MOVE_EVENT":
      return `[${timestamp}] Moved: ${event.source} -> ${event.destination}`;
    case "DELETE_EVENT":
      return `[${timestamp}] Deleted: ${event.fullPath}`;
    case "KEYMAP_RELOAD_EVENT":
      return `[${timestamp}] Keymap reloaded.`;
  }
};

/**
 * make item for a log entry
 */
const makeItem = (event: LogEvents, index: number) => {
  let level = "info";

  const entry = logEventToEntry(event);
  return (
    <div role="listitem" class="log-viewer__log-item" data-testid="logViewer-logItem" key={index}>
      <span class="log-viewer__item-level" data-testid="logViewer-itemLevel" data-level={level}>
        [{level.toUpperCase()}]
      </span>
      <span class="log-viewer__item-message" data-testid="logViewer-itemMessage">
        {entry}
      </span>
    </div>
  );
};

export const Component: preact.FunctionComponent<Props> = ({ entries, hidden }) => {
  let components: preact.VNode<any>[] = [];
  if (!hidden) {
    components = entries.map(makeItem);
  }

  return (
    <div class="log-viewer__root" data-testid="logViewer-root" aria-hidden={hidden}>
      {components}
    </div>
  );
};
