import * as S from "./selectors";
import { Configuration } from "@/generated/configuration_pb";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Selectors", () => {
      test("get current theme", () => {
        const state = new Configuration();
        state.setCurrentTheme("test");

        const theme = S.getCurrentTheme({ configuration: state.toObject() });
        expect(theme).toEqual("theme__test");
      });

      test("get default theme when configuration not initialized", () => {
        const theme = S.getCurrentTheme({ configuration: undefined });
        expect(theme).toEqual("theme__default");
      });
    });
  });
});
