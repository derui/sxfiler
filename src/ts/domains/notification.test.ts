import { createMessage, createProgress, Level, NotificationType } from "./notification";

describe("Notification", () => {
  it("can create progress item via factory", () => {
    const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

    expect(data.body.kind).toEqual(NotificationType.Progress);
  });

  it("can get body as progress from progress notification", () => {
    const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

    expect(data.getProgressBody()).toEqual({
      kind: NotificationType.Progress,
      process: "foo",
      current: 0,
      target: 100,
    });
  });

  it("throw error for progress notification to get body as message", () => {
    const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

    expect(() => data.getMessageBody()).toThrow();
  });

  it("can create one-shot item via factory", () => {
    const data = createMessage("id", Level.Info, "message");

    expect(data.getMessageBody()).toEqual({
      kind: NotificationType.Message,
      message: "message",
    });
  });

  it("throw error for message notification to get body as progress", () => {
    const data = createMessage("id", Level.Info, "message");

    expect(() => data.getProgressBody()).toThrow();
  });
});
