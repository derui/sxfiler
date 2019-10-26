import { SuggestionKind, createSuggestions } from "./task-suggestion";

describe("Domain", () => {
  describe("Task suggestion", () => {
    it("create suggestions from tasks", () => {
      const data = createSuggestions({
        taskId: "task",
        itemName: "node",
        suggestions: [SuggestionKind.Overwrite, SuggestionKind.Rename],
      });

      expect(data.taskId).toEqual("task");
      expect(data.suggestions).toHaveLength(2);
    });
  });
});
