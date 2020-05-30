import { actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";
import { ActionTypes } from "./types";
import { createSection } from "@/configurations/creators";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Actions", () => {
      test("create update action", () => {
        const config = new Configuration();

        const action = actions.update([config]);

        expect(action.type).toEqual(ActionTypes.UPDATE);
        expect(action.payload).toEqual({ config: [config] });
      });

      test("create select section action", () => {
        const section = createSection({
          key: ["foo", "bar"],
          displayName: "bar",
          description: "desc",
          items: [],
        });

        const action = actions.selectSection(section);

        expect(action.type).toEqual(ActionTypes.SELECT_SECTION);
        expect(action.payload).toEqual({ section });
      });
    });
  });
});
