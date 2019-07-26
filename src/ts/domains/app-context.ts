import { UIContext } from "@/types/ui-context";

export type AppContext = {
  readonly current: UIContext;
  readonly subContexts: UIContext[];
};

/**
   create AppContext
 */
export const createAppContext = ({
  current,
  subContexts,
}: {
  current: UIContext;
  subContexts?: UIContext[];
}): AppContext => {
  return {
    current,
    subContexts: Array.from(subContexts || []),
  };
};

/**
   change context
 */
export const changeCurrent = (context: UIContext) => (state: AppContext) => {
  return createAppContext({
    current: context,
    subContexts: state.subContexts,
  });
};

/**
   Add a context to AppContext
 */
export const addSubContext = (context: UIContext) => (state: AppContext) => {
  const tmpSet = new Set(state.subContexts);
  tmpSet.add(context);

  return createAppContext({
    current: state.current,
    subContexts: Array.from(tmpSet.values()),
  });
};

/**
   remove the context from AppContext
 */
export const removeSubContext = (context: UIContext) => (state: AppContext) => {
  const tmpSet = new Set(state.subContexts);
  tmpSet.delete(context);

  return createAppContext({
    current: state.current,
    subContexts: Array.from(tmpSet.values()),
  });
};
