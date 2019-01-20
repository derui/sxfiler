// reducers for notification
import { actions } from "../actions/notification";
import { createOneShot, Level } from "../domains/notification";
import Notifications from "../domains/notifications";
import reducer from "./notification";

describe("reducers", () => {
  describe("Notification state", () => {
    it("remove the notification specified id in Timeout action", () => {
      const state = { notifications: new Notifications([createOneShot("id", Level.Info, "message")]) };

      const ret = reducer(state, actions.timeout("id"));

      expect(ret.notifications).toEqual(new Notifications());
    });

    it("should append a new notification when Notify action", () => {
      const state = { notifications: new Notifications() };

      const data = createOneShot("id", Level.Info, "message");
      const ret = reducer(state, actions.notify(createOneShot("id", Level.Info, "message")));

      expect(ret.notifications.findById("id")).toEqual(data);
    });
  });
});
