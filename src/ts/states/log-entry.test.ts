import { empty, pushEntry } from "./log-entry";
import { createMessage, Level } from "../domains/message-notification";

describe("States", () => {
  describe("Log entries", () => {
    it("get empty state", () => {
      const state = empty();

      expect(state.entries.size).toEqual(0);
    });

    it("can push new entry", () => {
      const state = empty();
      const newState = pushEntry(state, createMessage({ id: "id", level: Level.Info, message: "message" }));

      expect(newState.entries.size).toEqual(1);
    });

    it("return new instance when push entry", () => {
      const state = empty();
      const newState = pushEntry(state, createMessage({ id: "id", level: Level.Info, message: "message" }));

      expect(state).not.toBe(newState);
    });
  });
});
