import { createMessage, createProgress, Level, NotificationKind } from "./notification";

describe("Domain", () => {
  describe("Notification", () => {
    it("can create progress item via factory", () => {
      const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

      expect(data.body.kind).toEqual(NotificationKind.Progress);
    });

    it("can get body as progress from progress notification", () => {
      const data = createProgress("id", Level.Info, { process: "foo", current: 0, target: 100 });

      expect(data.body).toEqual({
        kind: NotificationKind.Progress,
        process: "foo",
        current: 0,
        target: 100,
      });
    });

    it("can create one-shot item via factory", () => {
      const data = createMessage("id", Level.Info, "message");

      expect(data.body).toEqual({
        kind: NotificationKind.Message,
        message: "message",
      });
    });
  });
});
