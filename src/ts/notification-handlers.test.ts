import * as H from "./notification-handlers";
import { ContextLike } from "./context";
import { Level } from "./domains/message-notification";

describe("Notification handlers", () => {
  describe("Notify handler", () => {
    let executor = jest.fn();
    let context: ContextLike = {
      use: useCase => {
        return executor;
      },
    };

    afterEach(() => jest.clearAllMocks());

    it("throw error when invalid object schema", () => {
      expect(() => H.handleMessageNotification(context)({})).toThrowError();
      expect(() => H.handleMessageNotification(context)({ id: "foo" })).toThrowError();
      expect(() => H.handleMessageNotification(context)({ level: Level.Info })).toThrowError();
      expect(() => H.handleMessageNotification(context)({ body: "foo" })).toThrowError();
    });

    it("return notification domain for message of front", () => {
      const obj = {
        id: "id",
        level: Level.Info,
        body: "message",
      };
      H.handleMessageNotification(context)(obj);

      expect(executor).toBeCalled();
    });

    it("return notification domain for progress of front", () => {
      const obj = {
        id: "id",
        body: { process: "process", current: 0, target: 100 },
      };
      H.handleProgressNotification(context)(obj);

      expect(executor).toBeCalled();
    });
  });
});
