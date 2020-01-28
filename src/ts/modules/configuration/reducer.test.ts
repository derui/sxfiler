import { reducer, emptyState } from "./reducer";
import { actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Reducer", () => {
      test("update configuration given", () => {
        const config = new Configuration();
        const state = reducer(emptyState, actions.update(config));

        expect(state.configuration).toEqual(config.toObject());
      });
    });
  });
});
