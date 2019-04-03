import { createMessage, Level } from "./notification";
import { createNotifications } from "./notifications";

describe("Notification", () => {
  it("can find the notification by id", () => {
    const data = createNotifications(["id1", "id2", "id3"].map(id => createMessage(id, Level.Info, `${id} message`)));

    expect(data.findById("id2")).toEqual(createMessage("id2", Level.Info, `id2 message`));
  });

  it("return undefind when not found notification", () => {
    const data = createNotifications(["id1", "id2", "id3"].map(id => createMessage(id, Level.Info, `${id} message`)));

    expect(data.findById("id")).toBeUndefined();
  });

  it("return new instance when append", () => {
    const data = createNotifications([]);
    const newData = data.append(createMessage("id2", Level.Info, `id2 message`));

    expect(data).not.toBe(newData);
  });

  it("can append item", () => {
    const data = createNotifications([createMessage("id", Level.Info, "message")]);
    const newData = data.append(createMessage("id2", Level.Info, `id2 message`));

    expect(newData.findById("id2")).toEqual(createMessage("id2", Level.Info, `id2 message`));
    expect(newData.findById("id")).not.toBeUndefined();
  });

  it("can remove item", () => {
    const data = createNotifications([createMessage("id", Level.Info, "message")]);
    const newData = data.remove("id");

    expect(newData).not.toBe(data);
    expect(newData.findById("id")).toBeUndefined();
  });

  it("return new instance when not found id", () => {
    const data = createNotifications([createMessage("id", Level.Info, "message")]);
    const newData = data.remove("id2");

    expect(newData).not.toBe(data);
    expect(newData.findById("id")).not.toBeUndefined();
  });
});
