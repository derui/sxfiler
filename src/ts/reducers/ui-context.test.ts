// reducers for notification
import { actions } from "../actions/ui-context";
import UIContext from "../types/ui-context";
import reducer from "./ui-context";

describe("reducers", () => {
  describe("UI Context state", () => {
    it("should change state to OnFileTree", () => {
      const ret = reducer(UIContext.OnComplete, actions.enableFileTree());

      expect(ret).toBe(UIContext.OnFileTree);
    });

    it("should append a new notification when Notify action", () => {
      const ret = reducer(UIContext.OnComplete, actions.enablePreview());

      expect(ret).toBe(UIContext.OnPreview);
    });
  });
});
