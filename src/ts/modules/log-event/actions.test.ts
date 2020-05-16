import { actions } from "./actions";
import { ActionTypes } from "./types";
import { LogEventCreators } from "./reducer";

describe("Modules", () => {
  describe("Log Event", () => {
    describe("Actions", () => {
      test("create action to send log event", () => {
        const event = LogEventCreators.createDeleteItem(new Date(), "fullPath");

        expect(actions.send(event)).toEqual({
          type: ActionTypes.SEND,
          payload: event,
        });
      });
    });
  });
});
