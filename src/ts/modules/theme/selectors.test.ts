import * as S from "./selectors";
import { emptyState } from "./reducer";
import { also } from "@/libs/fn";
import { Theme, ColorCode } from "@/generated/theme_pb";
import { defaultTheme } from "./default-theme";

describe("Modules", () => {
  describe("Theme", () => {
    describe("Selectors", () => {
      test("return empty pairs if not found current theme", () => {
        const emptyColorPairs = S.selectColorPairs(emptyState, "");

        const pairs = defaultTheme()
          .getColorCodesList()
          .reduce((accum, v) => {
            accum[v.getName()] = v.getHexColor();
            return accum;
          }, {} as { [p: string]: string });

        expect(emptyColorPairs).toEqual(pairs);
      });

      test("return color pairs if not found current theme", () => {
        const theme = also(new Theme(), (v) => {
          v.setName("theme");
          v.addColorCodes(
            also(new ColorCode(), (v) => {
              v.setName("foo");
              v.setHexColor("#ffa");
            })
          );
          v.addColorCodes(
            also(new ColorCode(), (v) => {
              v.setName("bar");
              v.setHexColor("#a00");
            })
          );
        });
        const emptyColorPairs = S.selectColorPairs({ themes: [theme] }, "theme");

        expect(emptyColorPairs).toEqual({
          foo: "#ffa",
          bar: "#a00",
        });
      });
    });
  });
});
