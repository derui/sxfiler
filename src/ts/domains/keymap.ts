/**
 * keymap defines interfaces for key-action mappings on server.
 */

export type Binding = {
  key: string;
  action: string;
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
};

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
  } as Keymap & {
    _bindings: Binding[];
  };
};
