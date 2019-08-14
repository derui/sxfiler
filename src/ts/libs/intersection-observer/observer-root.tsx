import * as React from "react";
import * as observer from "./observer-manager";

export interface Props {
  children: (rootId: observer.RootId) => React.ReactNode;
}

interface State {
  rootId?: observer.RootId;
}

export const Component: React.FC<Props> = () => {
  const [state, setState] = React.useState<observer.RootId | undefined>(undefined);

  React.useEffect(() => {
    setState(observer.createRoot());

    return () => {
      if (state) {
        observer.deleteRoot(state);
      }
    };
  }, [setState]);

  if (!state) {
    return null;
  }
  return this.props.children(state);
};
