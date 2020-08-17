import { actions } from "./actions";
import { ActionTypes } from "./types";
import { ColorTheme } from "@/generated/theme_pb";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Actions", () => {
      test("create action to update list", () => {
        const theme = new ColorTheme();
        const action = actions.update(new ColorTheme());

        expect(action).toEqual({ type: ActionTypes.UPDATE, payload: { theme } });
      });
    });
  });
});
