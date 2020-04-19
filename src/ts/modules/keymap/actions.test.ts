import { actions } from "./actions";
import { Keymap } from "@/generated/keymap_pb";
import { ActionTypes } from "./types";
import { UIContext } from "@/types/ui-context";

describe("Modules", () => {
  describe("Keymap", () => {
    describe("Actions", () => {
      it("create update action", () => {
        const keymap = new Keymap();
        const action = actions.update(keymap);

        expect(action).toEqual({ type: ActionTypes.UPDATE, payload: { keymap } });
      });

      it("create removeContexts action", () => {
        const action = actions.removeContexts([UIContext.ForFinder]);

        expect(action).toEqual({ type: ActionTypes.REMOVE_CONTEXTS, payload: { contexts: [UIContext.ForFinder] } });
      });

      it("create addContexts action", () => {
        const action = actions.addContexts([UIContext.ForFinder]);

        expect(action).toEqual({ type: ActionTypes.ADD_CONTEXTS, payload: { contexts: [UIContext.ForFinder] } });
      });

      it("create replaceContexts action", () => {
        const action = actions.replaceContext([UIContext.ForFinder]);

        expect(action).toEqual({ type: ActionTypes.REPLACE_CONTEXT, payload: { contexts: [UIContext.ForFinder] } });
      });
    });
  });
});
