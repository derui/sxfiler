import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { Keymap, Binding } from "@/generated/keymap_pb";
import { UIContext, toUIContext } from "@/types/ui-context";

type currentKeymap = { [key: string]: string };
// state of type. Please redefine to what you want.
export type State = {
  // global keymap
  globalKeymap: Keymap | undefined;
  // keymap of current context
  currentKeymap: currentKeymap;
  contexts: Set<UIContext>;
};

// empty state
export const emptyState: State = {
  globalKeymap: undefined,
  currentKeymap: {},
  contexts: new Set(),
};

// common function for Set
const isSuperset = function isSuperset<T>(set_: Set<T>, subset: Set<T>): boolean {
  for (let elem of subset) {
    if (!set_.has(elem)) {
      return false;
    }
  }
  return true;
};

// make difference set that extract setB from setA
const difference = function difference<T>(setA: Set<T>, setB: Set<T>): Set<T> {
  var _difference = new Set(setA);
  for (let elem of setB) {
    _difference.delete(elem);
  }
  return _difference;
};

const contextMatched = function contextMatched(contextSet: Set<UIContext>, binding: Binding): boolean {
  const contexts = binding.getContextsList();
  const contextsInBinding = new Set<UIContext>(
    contexts.map(toUIContext).reduce((accum, v) => {
      if (!v) {
        return accum;
      }
      accum.push(v);
      return accum;
    }, new Array<UIContext>())
  );

  return isSuperset(contextSet, contextsInBinding);
};

// extract current keymap from global keymap with contexts
const extractCurrentKeymap = function (contexts: Set<UIContext>, keymap: Keymap): currentKeymap {
  const currentKeymap: currentKeymap = {};

  keymap
    .getBindingsList()
    .filter((binding) => contextMatched(contexts, binding))
    .forEach((binding: Binding) => {
      const { key, action } = binding.toObject();
      if (!key || !action) {
        return;
      }
      currentKeymap[key] = action;
    });

  return currentKeymap;
};

// sub-reducer for REMOVE_CONTEXTS action
const removeContexts = function (state: State, contexts: UIContext[]) {
  const globalKeymap = state.globalKeymap;

  const newContexts = difference(state.contexts, new Set(contexts));

  if (!globalKeymap) {
    return { ...state, contexts: newContexts };
  }

  return {
    ...state,
    currentKeymap: extractCurrentKeymap(newContexts, globalKeymap),
    contexts: newContexts,
  };
};

// sub-reducer for ADD_CONTEXTS action
const addContexts = function (state: State, contexts: UIContext[]) {
  const globalKeymap = state.globalKeymap;

  const newContexts = new Set(state.contexts);
  for (const v of contexts) {
    newContexts.add(v);
  }

  if (!globalKeymap) {
    return { ...state, contexts: newContexts };
  }

  return {
    ...state,
    currentKeymap: extractCurrentKeymap(newContexts, globalKeymap),
    contexts: newContexts,
  };
};

const replaceContext = function (state: State, contexts: UIContext[]) {
  const globalKeymap = state.globalKeymap;

  const newContexts = new Set(contexts);

  if (!globalKeymap) {
    return { ...state, contexts: newContexts };
  }

  return {
    ...state,
    currentKeymap: extractCurrentKeymap(newContexts, globalKeymap),
    contexts: newContexts,
  };
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return {
        ...state,
        globalKeymap: action.payload.keymap,
        currentKeymap: extractCurrentKeymap(state.contexts, action.payload.keymap),
      };

    case ActionTypes.ADD_CONTEXTS:
      return addContexts(state, action.payload.contexts);

    case ActionTypes.REMOVE_CONTEXTS:
      return removeContexts(state, action.payload.contexts);

    case ActionTypes.REPLACE_CONTEXT:
      return replaceContext(state, action.payload.contexts);

    default:
      return state;
  }
};
