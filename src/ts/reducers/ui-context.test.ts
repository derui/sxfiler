// reducers for notification
import { actions } from "../actions/ui-context";
import { actions as otherActions } from "../actions/notification";
import UIContext from "../types/ui-context";
import reducer from "./ui-context";

describe("reducers", () => {
  describe("UI Context state", () => {
    it("should change state to OnFileTree", () => {
      const ret = reducer(UIContext.OnComplete, actions.enableFileTree());

      expect(ret).toEqual(UIContext.OnFileTree);
    });

    it("should append a new notification when Notify action", () => {
      const ret = reducer(UIContext.OnComplete, actions.enablePreview());

      expect(ret).toEqual(UIContext.OnPreview);
    });

    it("should through action if unknown target", () => {
      const ret = reducer(UIContext.OnComplete, otherActions.timeout("id"));

      expect(ret).toBe(UIContext.OnComplete);
    });
  });
});
