import { UIContext } from "@/types/ui-context";

export type AppContextObject = {
  readonly current: UIContext;
  readonly subContexts: UIContext[];
};

export type AppContext = AppContextObject & {
  /**
     get the plain object
   */
  plain(): AppContextObject;

  /**
     change current context
   */
  changeCurrent(context: UIContext): AppContext;

  /**
     add to a context to sub contexts
   */
  addSubContext(context: UIContext): AppContext;

  /**
     remove from the context to sub contexts
   */
  removeSubContext(context: UIContext): AppContext;
};

type InnerAppContext = AppContext & {
  _subContexts: Set<UIContext>;
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
    get subContexts() {
      return Array.from(subContexts || []);
    },
    plain() {
      return { current: this.current, subContexts: Array.from(this._subContexts.values()) };
    },
    changeCurrent,
    addSubContext,
    removeSubContext,

    _subContexts: new Set(subContexts),
  } as InnerAppContext;
};

/**
   change context
 */
function changeCurrent(this: InnerAppContext, context: UIContext) {
  return createAppContext({
    current: context,
    subContexts: this.subContexts,
  });
}

/**
   Add a context to AppContext
 */
function addSubContext(this: InnerAppContext, context: UIContext) {
  const tmpSet = new Set(this.subContexts);
  tmpSet.add(context);

  return createAppContext({
    current: this.current,
    subContexts: Array.from(tmpSet.values()),
  });
}

/**
   remove the context from AppContext
 */
function removeSubContext(this: InnerAppContext, context: UIContext) {
  const tmpSet = new Set(this._subContexts);
  tmpSet.delete(context);

  return createAppContext({
    current: this.current,
    subContexts: Array.from(tmpSet.values()),
  });
}
