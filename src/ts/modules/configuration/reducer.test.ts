import { reducer, emptyState } from "./reducer";
import { actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Reducer", () => {
      test("update configuration given", () => {
        const config = new Configuration();
        config.setKeyList(["key", "list"]);
        config.setJsonValue(JSON.stringify({ value: 100 }));
        const state = reducer(emptyState, actions.update([config]));

        expect(state.configuration).toEqual({ "key.list": 100 });
      });
    });
  });
});
