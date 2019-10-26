// reducers for notification
import * as otherActions from "@/actions/notification";
import * as actions from "@/actions/task";
import * as completerActions from "@/actions/completer";
import { UIContext } from "@/types/ui-context";
import { reducer } from "./app-context";
import { createSuggestions } from "@/domains/task-suggestion";
import { createAppContext } from "@/domains/app-context";

describe("reducers", () => {
  describe("UI Context state", () => {
    it("should change state to OnFileTree", () => {
      const state = createAppContext({ current: UIContext.OnSuggestion });
      const ret = reducer(state, actions.finished("task"));

      const expected = createAppContext({ current: UIContext.OnFileTree });
      expect(ret).toEqual(expected);
    });

    it("should change state to do action for suggestion", () => {
      const ret = reducer(
        undefined,
        actions.requireInteraction(
          createSuggestions({
            taskId: "task",
            itemName: "node",
            suggestions: [],
          })
        )
      );

      expect(ret.current).toEqual(UIContext.OnSuggestion);
    });

    it("should through action if unknown target", () => {
      const ret = reducer(undefined, otherActions.timeout("id"));

      expect(ret.current).toBe(UIContext.OnFileTree);
    });

    it("should make current context to OnCompletion when completer opened", () => {
      const ret = reducer(undefined, completerActions.open("title", UIContext.ForHistory));
      const expected = createAppContext({
        current: UIContext.OnCompletion,
        subContexts: [UIContext.ForHistory],
      });

      expect(ret).toEqual(expected);
    });

    it("should make current context to OnFileTree when completer closed", () => {
      const ret = reducer(
        reducer(undefined, completerActions.open("title", UIContext.ForHistory)),
        completerActions.close(UIContext.ForHistory)
      );
      const expected = createAppContext({
        current: UIContext.OnFileTree,
      });

      expect(ret).toEqual(expected);
    });
  });
});
