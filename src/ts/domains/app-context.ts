import { UIContext } from "@/types/ui-context";

export type AppContext = {
  readonly current: UIContext;
  readonly subContexts: UIContext[];
};

/**
   create AppContext
 */
export function createAppContext({
  current,
  subContexts,
}: {
  current: UIContext;
  subContexts?: UIContext[];
}): AppContext {
  return {
    current,
    subContexts: Array.from(subContexts || []),
  };
}

/**
   change context
 */
export function changeCurrent(context: UIContext) {
  return (state: AppContext) =>
    createAppContext({
      current: context,
      subContexts: state.subContexts,
    });
}

/**
   Add a context to AppContext
 */
export function addSubContext(context: UIContext) {
  return (state: AppContext) => {
    const tmpSet = new Set(state.subContexts);
    tmpSet.add(context);

    return createAppContext({
      current: state.current,
      subContexts: Array.from(tmpSet.values()),
    });
  };
}

/**
   remove the context from AppContext
 */
export function removeSubContext(context: UIContext) {
  return (state: AppContext) => {
    const tmpSet = new Set(state.subContexts);
    tmpSet.delete(context);

    return createAppContext({
      current: state.current,
      subContexts: Array.from(tmpSet.values()),
    });
  };
}
