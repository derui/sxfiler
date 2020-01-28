import { h } from "preact";

export type Props = {
  entries: string[];
  hidden: boolean;
};

/**
 * make item for a log entry
 */
const makeItem = (entry: string) => {
  let level = "info";

  return (
    <div role="listitem" class="log-viewer__log-item" data-testid="logViewer-logItem" key={entry}>
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
