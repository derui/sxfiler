import { SuggestionKind, createSuggestion, createSuggestions } from "./task-suggestion";

describe("Domain", () => {
  describe("Task suggestion", () => {
    it("can create suggestion", () => {
      const data = createSuggestion({
        kind: SuggestionKind.Overwrite,
      });

      expect(data.kind).toEqual(SuggestionKind.Overwrite);
    });

    it("create suggestions from tasks", () => {
      const overwrite = createSuggestion({
        kind: SuggestionKind.Overwrite,
      });

      const rename = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const data = createSuggestions({ taskId: "task", nodeName: "node", suggestions: [overwrite, rename] });

      expect(data.taskId).toEqual("task");
      expect(data.suggestions).toHaveLength(2);
    });
  });
});
