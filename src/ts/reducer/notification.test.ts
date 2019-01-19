// reducers for notification
import { actions } from "../actions/notification";
import { createOneShot, Level } from "../domain/notification";
import Notifications from "../domain/notifications";
import reducer from "./notification";

describe("reducers", () => {
  describe("Notification state", () => {
    it("remove the notification specified id in Timeout action", () => {
      const state = { notifications: new Notifications([createOneShot("id", Level.Info, "message")]) };

      const ret = reducer(state, actions.timeout("id"));

      expect(ret.notifications).toEqual(new Notifications());
    });
  });
});
