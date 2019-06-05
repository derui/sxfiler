// reducers for notification
import { actions as otherActions } from "../actions/notification";
import { actions } from "../actions/ui-context";
import UIContext from "../types/ui-context";
import reducer from "./ui-context";
import * as Keymap from "../domains/keymap";

describe("reducers", () => {
  describe("UI Context state", () => {
    const keymap = Keymap.createKeymap();
    it("should change state to OnFileTree", () => {
      const ret = reducer(UIContext.OnComplete, actions.enableFileTree({ keymap }));

      expect(ret).toEqual(UIContext.OnFileTree);
    });

    it("should change state to do action for suggestion", () => {
      const ret = reducer(UIContext.OnComplete, actions.enableSuggestion({ keymap }));

      expect(ret).toEqual(UIContext.OnSuggestion);
    });

    it("should through action if unknown target", () => {
      const ret = reducer(UIContext.OnComplete, otherActions.timeout("id"));

      expect(ret).toBe(UIContext.OnComplete);
    });
  });
});
