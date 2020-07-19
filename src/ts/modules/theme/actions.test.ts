import { actions } from "./actions";
import { ActionTypes } from "./types";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Actions", () => {
      test("create action to update list", () => {
        const action = actions.updateList([]);

        expect(action).toEqual({ type: ActionTypes.UPDATE_LIST, payload: { themes: [] } });
      });
    });
  });
});
