import { createMessage, Level } from "./message-notification";

describe("Domain", () => {
  describe("Notification", () => {
    it("can create one-shot item via factory", () => {
      const data = createMessage({ id: "id", level: Level.Info, message: "message" });

      expect(data.body).toEqual("message");
    });
  });
});
