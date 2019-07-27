import { ProgressNotification } from "./progress-notification";

export type ProgressNotifications = {
  // get notifications as array
  readonly values: { [id: string]: ProgressNotification };
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
    values: _items,
  };
};

/**
   get values as array
 */
export const asArray = (state: ProgressNotifications): ProgressNotification[] => Object.values(state.values);

/**
 * Remove the notification having id
 * @param id the id of notification to want to remove
 */
export const remove = (id: string) => (state: ProgressNotifications): ProgressNotifications => {
  const data = { ...state.values };
  delete data[id];

  return createNotifications(Object.values(data));
};

/**
 * append a notification and return new instance.
 * @param item
 */
export const append = (item: ProgressNotification) => (state: ProgressNotifications): ProgressNotifications => {
  return createNotifications(Object.values({ ...state.values, [item.id]: item }));
};
