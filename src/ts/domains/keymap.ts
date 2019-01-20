/**
 * keymap defines interfaces for key-action mappings on server.
 */

export interface Binding {
  key: string;
  action: string;
}

export class Keymap {
  private _bindings: Binding[] = [];
  constructor(bindings: Binding[] = []) {
    this._bindings = bindings.map(v => v);
  }

  // getter for bindings
  get bindings() {
    return this._bindings.map(v => v);
  }

  /**
   * find the binding of the key
   * @param key the key to find the binding
   */
  public find(key: string): Binding | null {
    const filtered = this._bindings.filter(v => v.key === key);

    if (filtered.length === 0) {
      return null;
    }

    return filtered[0];
  }
}
