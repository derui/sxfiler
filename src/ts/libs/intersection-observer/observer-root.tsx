import * as React from "react";
import * as observer from "./observer-manager";

export type Props = {
  children: (rootId: observer.RootId) => React.ReactElement;
};

export const Component: React.FC<Props> = props => {
  const rootId = React.useRef("");
  const [state, setState] = React.useState<observer.RootId>("");

  React.useEffect(() => {
    const id = observer.createRoot();
    setState(id);
    rootId.current = id;

    return () => {
      observer.deleteRoot(rootId.current);
    };
  }, [setState]);

  if (!state) {
    return null;
  }
  return props.children(state);
};
