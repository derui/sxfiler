// reducers for notification
import { actions } from "../actions/notification";
import { actions as otherActions } from "../actions/ui-context";
import { createMessage, Level } from "../domains/notification";
import Notifications from "../domains/notifications";
import { empty, State } from "../types/store-state/notification";
import reducer from "./notification";

describe("reducers", () => {
  describe("Notification state", () => {
    it("remove the notification specified id in Timeout action", () => {
      const state: State = {
        notifications: new Notifications([createMessage("id", Level.Info, "message")]),
        timeouts: new Notifications(),
      };

      const ret = reducer(state, actions.timeout("id"));

      expect(ret).toEqual({
        notifications: state.notifications,
        timeouts: state.notifications,
      });
    });

    it("should append a new notification when ReceiveNotification action", () => {
      const state = empty();

      const data = createMessage("id", Level.Info, "message");
      const ret = reducer(state, actions.receiveNotification(createMessage("id", Level.Info, "message")));

      expect(ret.notifications.findById("id")).toEqual(data);
    });

    it("should remove totally when called with Remove action", () => {
      const state: State = {
        notifications: new Notifications([createMessage("id", Level.Info, "message")]),
        timeouts: new Notifications([createMessage("id", Level.Info, "message")]),
      };

      const ret = reducer(state, actions.remove("id"));

      expect(ret).toEqual(empty());
    });

    it("should not append to list of timeouts if unknown id given", () => {
      const state: State = {
        notifications: new Notifications([createMessage("id", Level.Info, "message")]),
        timeouts: new Notifications(),
      };

      const ret = reducer(state, actions.timeout("unknown"));

      expect(ret.timeouts).toEqual(new Notifications());
    });

    it("should through unhandle action", () => {
      const state: State = {
        notifications: new Notifications([createMessage("id", Level.Info, "message")]),
        timeouts: new Notifications(),
      };

      const ret = reducer(state, otherActions.enableFileTree());

      expect(ret).toStrictEqual(state);
    });
  });
});
