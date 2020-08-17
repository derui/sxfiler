import { reducer, emptyState } from "./reducer";
import { actions } from "./actions";
import { pipe } from "@/libs/fn";
import { ColorTheme, ColorPair } from "@/generated/theme_pb";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Reducer", () => {
      test("update theme", () => {
        const theme = new ColorTheme();
        theme.addColorPairs(new ColorPair());

        const state = pipe((v) => reducer(v, actions.update(theme)))(emptyState);

        expect(state.theme).toEqual(theme);
      });
    });
  });
});
