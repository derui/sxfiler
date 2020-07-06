import { reducer, emptyState } from "./reducer";
import { actions } from "./actions";
import { pipe } from "@/libs/fn";
import { Theme } from "@/generated/theme_pb";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Reducer", () => {
      test("change theme list", () => {
        const theme = new Theme();
        theme.setName("foobar");

        const state = pipe((v) => reducer(v, actions.updateList([theme])))(emptyState);

        expect(state.themes).toEqual([theme]);
      });
    });
  });
});
