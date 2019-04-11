// reducers for notification
import { actions } from "../actions/keymap";
import { empty, State } from "../states/keymap";
import reducer from "./keymap";
import { createKeymap } from "../domains/keymap";

describe("reducers", () => {
  describe("Keymap state", () => {
    it("return new state when get the keymap", () => {
      const state: State = empty();
      const newKeymap = createKeymap([{ key: "a", action: "actions" }]);

      const ret = reducer(state, actions.getKeymap({ keymap: newKeymap }));

      expect(ret).toEqual({ current: newKeymap });
    });
  });
});
