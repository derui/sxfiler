/**
 * keymap defines interfaces for key-action mappings on server.
 */

export interface Binding {
  key: string;
  action: string;
}

export class Keymap {
  constructor(private _bindings: Binding[]) {}

  // getter for bindings
  get bindings() {
    return this._bindings.map(v => v);
  }

  /**
   * find the binding of the key
   * @param key the key to find the binding
   */
  find(key: string) : Binding | null {
    const filtered = this._bindings.filter(v => v.key === key);

    if (filtered.length === 0) {
      return null;
    }

    return filtered[0];
  }
}
