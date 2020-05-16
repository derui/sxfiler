import * as S from "./selectors";
import { emptyState } from "./reducer";

describe("Modules", () => {
  describe("Log Event", () => {
    describe("Selectors", () => {
      test("get all events", () => {
        const state = emptyState;
        expect(S.allEvents(state)).toEqual([]);
      });
    });
  });
});
