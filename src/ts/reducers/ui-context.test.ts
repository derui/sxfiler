// reducers for notification
import { actions as otherActions } from "@/actions/notification";
import { actions } from "@/actions/task";
import { UIContext } from "@/types/ui-context";
import { reducer } from "./ui-context";
import { createSuggestions } from "@/domains/task-suggestion";

describe("reducers", () => {
  describe("UI Context state", () => {
    it("should change state to OnFileTree", () => {
      const ret = reducer(UIContext.OnSuggestion, actions.finished("task"));

      expect(ret).toEqual(UIContext.OnFileTree);
    });

    it("should change state to do action for suggestion", () => {
      const ret = reducer(
        UIContext.OnFileTree,
        actions.requireInteraction(
          createSuggestions({
            taskId: "task",
            nodeName: "node",
            suggestions: [],
          })
        )
      );

      expect(ret).toEqual(UIContext.OnSuggestion);
    });

    it("should through action if unknown target", () => {
      const ret = reducer(UIContext.OnFileTree, otherActions.timeout("id"));

      expect(ret).toBe(UIContext.OnFileTree);
    });
  });
});
