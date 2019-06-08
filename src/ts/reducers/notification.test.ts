// reducers for notification
import { actions } from "../actions/notification";
import { createMessage, Level } from "../domains/notification";
import { createNotifications } from "../domains/notifications";
import { empty, State } from "../states/notification";
import reducer from "./notification";
import { createKeymap } from "../domains/keymap";

describe("reducers", () => {
  describe("Notification state", () => {
    it("remove the notification specified id in Timeout action", () => {
      const state: State = {
        notifications: createNotifications([createMessage("id", Level.Info, "message")]),
        timeouts: createNotifications([]),
      };

      const ret = reducer(state, actions.timeout("id"));

      expect(ret.notifications.messages).toHaveLength(1);
      expect(ret.timeouts.messages).toHaveLength(1);
    });

    it("should append a new notification when ReceiveNotification action", () => {
      const state = empty();

      const data = createMessage("id", Level.Info, "message");
      const ret = reducer(state, actions.receiveNotification(createMessage("id", Level.Info, "message")));

      expect(ret.notifications.findById("id")).toEqual(data);
    });

    it("should remove totally when called with Remove action", () => {
      const state: State = {
        notifications: createNotifications([createMessage("id", Level.Info, "message")]),
        timeouts: createNotifications([createMessage("id", Level.Info, "message")]),
      };

      const ret = reducer(state, actions.remove("id"));

      expect(ret.notifications.messages).toHaveLength(0);
      expect(ret.notifications.progresses).toHaveLength(0);
      expect(ret.timeouts.messages).toHaveLength(0);
      expect(ret.timeouts.progresses).toHaveLength(0);
    });

    it("should not append to list of timeouts if unknown id given", () => {
      const state: State = {
        notifications: createNotifications([createMessage("id", Level.Info, "message")]),
        timeouts: createNotifications([]),
      };

      const ret = reducer(state, actions.timeout("unknown"));

      expect(ret.timeouts.messages).toHaveLength(0);
      expect(ret.timeouts.progresses).toHaveLength(0);
    });
  });
});
