import { UIContext } from "@/types/ui-context";
import { AppContext } from "./app-context";

/**
 * keymap defines interfaces for key-action mappings on server.
 */

type When = {
  contexts: UIContext[];
};

export type Binding = {
  key: string;
  action: string;
  when: When;
};

export type Keymap = {
  /**
   * current bindnings in key map
   */
  readonly bindings: Binding[];
};

const evaluateWithContext = function evaluateWithContext(w: When, context: AppContext) {
  const contextSet = new Set(context.subContexts);
  contextSet.add(context.current);

  return w.contexts.every(v => contextSet.has(v));
};

export const allowedInContext = function allowedInContext(context: AppContext) {
  return (state: Keymap): Keymap => {
    const evaluatedKeymap = state.bindings
      .filter(v => evaluateWithContext(v.when, context))
      .reduce((map, v) => {
        const binding = map.get(v.key);
        if (binding && binding.when.contexts.length < v.when.contexts.length) {
          map.set(v.key, v);
        } else {
          map.set(v.key, v);
        }

        return map;
      }, new Map<string, Binding>())
      .values();

    return createKeymap(Array.from(evaluatedKeymap));
  };
};

export const createKeymap = function createKeymap(bindings: Binding[] = []): Keymap {
  return {
    bindings: Array.from(bindings),
  };
};

export const find = function find(key: string) {
  return (state: Keymap): Binding | undefined => {
    return state.bindings.find(v => v.key === key);
  };
};
