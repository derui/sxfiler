import { Notification, NotificationKind, MessageNotification, ProgressNotification } from "./notification";

export type Notifications = {
  // get notifications as array
  messages: MessageNotification[];
  progresses: ProgressNotification[];

  /**
   * return matched notification
   * @param id ID of notification
   */
  findById(id: string): Notification | undefined;

  /**
   * Remove the notification having id
   * @param id the id of notification to want to remove
   */
  remove(id: string): Notifications;

  /**
   * append a notification and return new instance.
   * @param item
   */
  append(item: Notification): Notifications;
};

// Used only module internal type
type InnerDefinition = Notifications & {
  _items: { [id: string]: Notification };
};

/**
 * list implementation of Notification entity.
 * All method of this class is immutable.
 */
export const createNotifications = (notifications: Notification[]): Notifications => {
  const _items: { [id: string]: Notification } = {};
  notifications.forEach(v => {
    _items[v.id] = v;
  });

  return {
    _items,

    // get notifications as array
    get messages(): MessageNotification[] {
      return Object.values(this._items)
        .filter(v => v.kind === NotificationKind.Message)
        .map(v => v as MessageNotification);
    },
    get progresses(): ProgressNotification[] {
      return Object.values(this._items)
        .filter(v => v.kind === NotificationKind.Progress)
        .map(v => v as ProgressNotification);
    },

    /**
     * return matched notification
     * @param id ID of notification
     */
    findById(id: string): Notification | undefined {
      return this._items[id];
    },

    /**
     * Remove the notification having id
     * @param id the id of notification to want to remove
     */
    remove(id: string): Notifications {
      const data = { ...this._items };
      delete data[id];

      return createNotifications(Object.values(data));
    },

    /**
     * append a notification and return new instance.
     * @param item
     */
    append(item: Notification): Notifications {
      return createNotifications(Object.values({ ...this._items, [item.id]: item }));
    },
  } as InnerDefinition;
};
