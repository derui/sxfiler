// reducers for notification
import { actions } from "@/actions/notification";
import { empty, State } from "@/states/notification";
import { reducer } from "./notification";
import { createProgress } from "@/domains/progress-notification";
import { createNotifications, asArray } from "@/domains/progress-notifications";

describe("reducers", () => {
  describe("Notification state", () => {
    const data = createProgress("id", {
      current: 1,
      process: "process",
      target: 100,
    });

    it("remove the notification specified id in Timeout action", () => {
      const state: State = {
        progresses: createNotifications([data]),
        timeouts: createNotifications([]),
      };

      const ret = reducer(state, actions.timeout("id"));

      expect(asArray(ret.progresses)).toHaveLength(1);
      expect(asArray(ret.timeouts)).toHaveLength(1);
    });

    it("should append a new notification when ReceiveNotification action", () => {
      const state = empty();

      const ret = reducer(state, actions.receiveProgress(data));

      expect(ret.progresses.values["id"]).toEqual(data);
    });

    it("should remove totally when called with Remove action", () => {
      const state: State = {
        progresses: createNotifications([data]),
        timeouts: createNotifications([data]),
      };

      const ret = reducer(state, actions.remove(data.id));

      expect(ret.progresses.values).toEqual({});
      expect(ret.timeouts.values).toEqual({});
    });
  });
});
