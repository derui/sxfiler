import * as H from "./notification-handlers";
import { NotificationKind, Level, createMessage, createProgress } from "./domains/notification";

describe("Notification handlers", () => {
  describe("Notify handler", () => {
    it("throw error when invalid object schema", () => {
      expect(() => H.handleNotification({})).toThrowError();
      expect(() => H.handleNotification({ id: "foo" })).toThrowError();
      expect(() => H.handleNotification({ type: NotificationKind.Message })).toThrowError();
      expect(() => H.handleNotification({ body: "foo" })).toThrowError();
    });

    it("throw error when passed unknown notification kind", () => {
      const obj = {
        id: "id",
        type: "unknown",
        level: Level.Info,
      };
      expect(() => H.handleNotification(obj)).toThrowError();
    });

    it("return notification domain for message of front", () => {
      const obj = {
        id: "id",
        level: Level.Info,
        body: {
          type: NotificationKind.Message,
          message: "message",
        },
      };
      expect(H.handleNotification(obj)).toEqual(createMessage("id", Level.Info, "message"));
    });

    it("return notification domain for progress of front", () => {
      const obj = {
        id: "id",
        level: Level.Info,
        body: {
          type: NotificationKind.Progress,
          progress: { process: "process", current: 0, target: 100 },
        },
      };
      expect(H.handleNotification(obj)).toEqual(
        createProgress("id", Level.Info, { process: "process", current: 0, target: 100 })
      );
    });
  });
});
