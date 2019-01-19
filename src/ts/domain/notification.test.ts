import { createOneShot, createProgress, Level, NotificationType } from "./notification";

describe("Notification", () => {
  it("can create progress item via factory", () => {
    const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

    expect(data.body.kind).toEqual(NotificationType.Progress);
  });

  it("can create one-shot item via factory", () => {
    const data = createOneShot("id", Level.Info, "message");

    expect(data.body.kind).toEqual(NotificationType.OneShot);
  });
});
