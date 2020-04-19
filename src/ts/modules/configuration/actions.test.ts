import { actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";
import { ActionTypes } from "./types";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Actions", () => {
      test("create update action", () => {
        const config = new Configuration();

        const action = actions.update(config);

        expect(action.type).toEqual(ActionTypes.UPDATE);
        expect(action.payload).toEqual({ config });
      });
    });
  });
});
