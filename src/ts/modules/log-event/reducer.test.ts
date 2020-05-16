import { actions } from "./actions";
import { emptyState, reducer, LogEventCreators } from "./reducer";

describe("Modules", () => {
  describe("Log Event", () => {
    describe("Reducer", () => {
      test("add event from any place", () => {
        const event = LogEventCreators.createKeymapReload(new Date());
        const state = reducer(emptyState, actions.send([event]));

        expect(state.events).toEqual([event]);
      });

      test("should append event always last", () => {
        const event1 = LogEventCreators.createKeymapReload(new Date());
        const event2 = LogEventCreators.createKeymapReload(new Date());
        let state = reducer(emptyState, actions.send([event1]));
        state = reducer(state, actions.send([event2]));

        expect(state.events).toEqual([event1, event2]);
      });
    });
  });
});
