import { createNotifications } from "./progress-notifications";
import { createProgress } from "./progress-notification";

describe("Notification", () => {
  const notification = createProgress("id", {
    current: 1,
    process: "test",
    target: 10,
  });

  it("can find the notification by id", () => {
    const data = createNotifications([notification]);

    expect(data.findById("id")).toEqual(notification);
  });

  it("return undefind when not found notification", () => {
    const data = createNotifications([notification]);

    expect(data.findById("id-not-exists")).toBeUndefined();
  });

  it("return new instance when append", () => {
    const data = createNotifications([]);
    const newData = data.append(notification);

    expect(data).not.toBe(newData);
  });

  it("can append item", () => {
    const data = createNotifications([]);
    const newData = data.append(notification);

    expect(newData.findById("id")).toEqual(notification);
  });

  it("can remove item", () => {
    const data = createNotifications([notification]);
    const newData = data.remove(notification.id);

    expect(newData).not.toBe(data);
    expect(newData.findById(notification.id)).toBeUndefined();
  });

  it("return new instance when not found id", () => {
    const data = createNotifications([notification]);
    const newData = data.remove("id2");

    expect(newData).not.toBe(data);
    expect(newData.findById(notification.id)).not.toBeUndefined();
  });
});
