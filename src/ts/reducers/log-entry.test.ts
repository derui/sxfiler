// reducers for notification
import { actions } from "../actions/notification";
import { empty } from "../states/log-entry";
import reducer from "./log-entry";
import { createMessage, Level } from "../domains/message-notification";

describe("reducers", () => {
  describe("Notification state", () => {
    const data = createMessage({ id: "id", level: Level.Info, message: "message" });

    it("should append a new log entry when message notification receieved", () => {
      const state = empty();

      const ret = reducer(state, actions.receiveMessage(data));

      expect(ret.entries.size).toEqual(1);
    });

    it("should over write when get the notification having same id already appended", () => {
      const state = empty();
      const newData = createMessage({ id: data.id, level: Level.Error, message: "error" });
      let ret = reducer(state, actions.receiveMessage(data));
      ret = reducer(ret, actions.receiveMessage(newData));

      expect(ret.entries.get(data.id)).toEqual(newData);
    });
  });
});
