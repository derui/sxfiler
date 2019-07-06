import UIContext from "../types/ui-context";
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
  bindings: Binding[];

  /**
   * find a binding of a key
   * @param key the key to find the binding
   */
  find(key: string): Binding | undefined;

  /**
   * Get subset of keymap allowed to use
   */
  allowedWhen(context: AppContext): Keymap;
};

const evaluateWithContext = (w: When, context: AppContext) => {
  const contextSet = new Set(context.subContexts);
  contextSet.add(context.current);

  return w.contexts.every(v => contextSet.has(v));
};

function allowedWhen(this: Keymap, context: AppContext): Keymap {
  const evaluatedKeymap = this.bindings
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
}

export const createKeymap = (bindings: Binding[] = []): Keymap => {
  return {
    _bindings: Array.from(bindings),

    // getter for bindings
    get bindings() {
      return Array.from(this._bindings);
    },

    find(key: string): Binding | undefined {
      return this._bindings.find(v => v.key === key);
    },
    allowedWhen,
  } as Keymap & {
    _bindings: Binding[];
  };
};
