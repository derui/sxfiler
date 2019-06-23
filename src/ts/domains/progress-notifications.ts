import { ProgressNotification } from "./progress-notification";

export type ProgressNotifications = {
  // get notifications as array
  notifications: ProgressNotification[];

  /**
   * return matched notification
   * @param id ID of notification
   */
  findById(id: string): ProgressNotification | undefined;

  /**
   * Remove the notification having id
   * @param id the id of notification to want to remove
   */
  remove(id: string): ProgressNotifications;

  /**
   * append a notification and return new instance.
   * @param item
   */
  append(item: ProgressNotification): ProgressNotifications;
};

// Used only module internal type
type InnerDefinition = ProgressNotifications & {
  _items: { [id: string]: ProgressNotification };
};

/**
 * list implementation of Notification entity.
 * All method of this class is immutable.
 */
export const createNotifications = (notifications: ProgressNotification[]): ProgressNotifications => {
  const _items: { [id: string]: ProgressNotification } = {};
  notifications.forEach(v => {
    _items[v.id] = v;
  });

  return {
    _items,

    get notifications(): ProgressNotification[] {
      return Object.values(this._items).map(v => v);
    },

    /**
     * return matched notification
     * @param id ID of notification
     */
    findById(id: string): ProgressNotification | undefined {
      return this._items[id];
    },

    /**
     * Remove the notification having id
     * @param id the id of notification to want to remove
     */
    remove(id: string): ProgressNotifications {
      const data = { ...this._items };
      delete data[id];

      return createNotifications(Object.values(data));
    },

    /**
     * append a notification and return new instance.
     * @param item
     */
    append(item: ProgressNotification): ProgressNotifications {
      return createNotifications(Object.values({ ...this._items, [item.id]: item }));
    },
  } as InnerDefinition;
};
