import { empty } from "./log-entry";

describe("States", () => {
  describe("Log entries", () => {
    it("get empty state", () => {
      const state = empty();

      expect(state.entries.size).toEqual(0);
    });
  });
});
