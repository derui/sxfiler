// reducers for notification
import { actions } from "@/actions/key-map";
import { empty, State } from "@/states/keymap";
import reducer from "./keymap";
import { createKeymap } from "@/domains/keymap";

describe("reducers", () => {
  describe("Keymap state", () => {
    it("return new state when get the keymap", () => {
      const state: State = empty();
      const newKeymap = createKeymap([{ key: "a", action: "actions", when: { contexts: [] } }]);

      const ret = reducer(state, actions.updateKeymap(newKeymap));

      expect(ret.allKeymap).toEqual(newKeymap);
      expect(ret.current).toEqual(newKeymap);
    });
  });
});
