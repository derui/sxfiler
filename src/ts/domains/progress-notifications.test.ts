import { createNotifications, append, remove } from "./progress-notifications";
import { createProgress } from "./progress-notification";

describe("Notification", () => {
  const notification = createProgress("id", {
    current: 1,
    process: "test",
    target: 10,
  });

  it("can find the notification by id", () => {
    const data = createNotifications([notification]);

    expect(data.values["id"]).toEqual(notification);
  });

  it("return undefind when not found notification", () => {
    const data = createNotifications([notification]);

    expect(data.values["id-not-exists"]).toBeUndefined();
  });

  it("return new instance when append", () => {
    const data = createNotifications([]);
    const newData = append(notification)(data);

    expect(data).not.toBe(newData);
  });

  it("can append item", () => {
    const data = createNotifications([]);
    const newData = append(notification)(data);

    expect(newData.values["id"]).toEqual(notification);
  });

  it("can remove item", () => {
    const data = createNotifications([notification]);
    const newData = remove(notification.id)(data);

    expect(newData).not.toBe(data);
    expect(newData.values[notification.id]).toBeUndefined();
  });

  it("return new instance when not found id", () => {
    const data = createNotifications([notification]);
    const newData = remove("id2")(data);

    expect(newData).not.toBe(data);
    expect(newData.values[notification.id]).not.toBeUndefined();
  });
});
