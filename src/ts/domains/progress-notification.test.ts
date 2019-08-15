import { createProgress } from "./progress-notification";

describe("Domain", () => {
  describe("Notification", () => {
    it("can get body as progress from progress notification", () => {
      const data = createProgress("id", { process: "foo", current: 0, targeted: 100 });

      expect(data.body).toEqual({
        process: "foo",
        current: 0,
        targeted: 100,
      });
    });
  });
});
