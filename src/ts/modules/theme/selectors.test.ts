import * as S from "./selectors";
import { emptyState } from "./reducer";
import { also } from "@/libs/fn";
import { ColorPair, ColorTheme } from "@/generated/theme_pb";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Selectors", () => {
      test("return empty pairs if any color pairs not have", () => {
        const emptyColorPairs = S.selectColorPairs(emptyState);

        expect(emptyColorPairs).toEqual({});
      });

      test("return color pairs if not found current theme", () => {
        const theme = also(new ColorTheme(), (v) => {
          v.addColorPairs(
            also(new ColorPair(), (v) => {
              v.setName("foo");
              v.setHexColor("#ffa");
            })
          );
          v.addColorPairs(
            also(new ColorPair(), (v) => {
              v.setName("bar");
              v.setHexColor("#a00");
            })
          );
        });
        const emptyColorPairs = S.selectColorPairs({ theme });

        expect(emptyColorPairs).toEqual({
          foo: "#ffa",
          bar: "#a00",
        });
      });
    });
  });
});
