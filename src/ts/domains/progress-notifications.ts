import { ProgressNotification } from "./progress-notification";

export type ProgressNotifications = {
  // get notifications as array
  readonly values: { [id: string]: ProgressNotification };
};

/**
 * list implementation of Notification entity.
 * All method of this class is immutable.
 */
export const createNotifications = function createNotifications(
  notifications: ProgressNotification[]
): ProgressNotifications {
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
export const asArray = function asArray(state: ProgressNotifications): ProgressNotification[] {
  return Object.values(state.values);
};

/**
 * Remove the notification having id
 * @param id the id of notification to want to remove
 */
export const remove = function remove(id: string) {
  return (state: ProgressNotifications) => {
    const data = { ...state.values };
    delete data[id];

    return createNotifications(Object.values(data));
  };
};

/**
 * append a notification and return new instance.
 * @param item
 */
export const append = function append(item: ProgressNotification) {
  return (state: ProgressNotifications) => createNotifications(Object.values({ ...state.values, [item.id]: item }));
};
