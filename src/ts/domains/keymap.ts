import UIContext from "../types/ui-context";

/**
 * keymap defines interfaces for key-action mappings on server.
 */

export type AppContext = {
  currentContext: UIContext;
};

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

const evaluateWithContext = (w: When, context: AppContext) => w.contexts.includes(context.currentContext);

function allowedWhen(this: Keymap, context: AppContext): Keymap {
  return createKeymap(this.bindings.filter(v => evaluateWithContext(v.when, context)));
}

export const createKeymap = (bindings: Binding[] = []): Keymap => {
  return {
    _bindings: Array.from(bindings),

    // getter for bindings
    get bindings() {
      return this._bindings.map(v => v);
    },

    find(key: string): Binding | undefined {
      return this._bindings.find(v => v.key === key);
    },
    allowedWhen,
  } as Keymap & {
    _bindings: Binding[];
  };
};
