import { Notification } from "./notification";

/**
 * list implementation of Notification entity.
 * All method of this class is immutable.
 */
export default class Notifications {
  private _items: { [id: string]: Notification } = {};
  constructor(items: Notification[] = []) {
    const self = this;
    items.forEach(v => (self._items[v.id] = v));
  }

  /**
   * return matched notification
   * @param id ID of notification
   */
  public findById(id: string): Notification | undefined {
    return this._items[id];
  }

  /**
   * Remove the notification having id
   * @param id the id of notification to want to remove
   */
  public remove(id: string): Notifications {
    const data = {...this._items};
    delete data[id];

    return new Notifications(Object.values(data));
  }

  /**
   * append a notification and return new instance.
   * @param item
   */
  public append(item: Notification): Notifications {
    const ret = new Notifications();
    ret._items = { ...this._items, [item.id]: item };
    return ret;
  }
}
