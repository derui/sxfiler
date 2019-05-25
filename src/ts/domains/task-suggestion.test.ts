import { SuggestionKind, createSuggestion, createSuggestions } from "./task-suggestion";

describe("Domain", () => {
  describe("Task suggestion", () => {
    it("can create suggestion", () => {
      const data = createSuggestion({
        kind: SuggestionKind.Overwrite,
        nodeName: "node",
      });

      expect(data.kind).toEqual(SuggestionKind.Overwrite);
      expect(data.nodeName).toEqual("node");
    });

    it("create suggestions from tasks", () => {
      const overwrite = createSuggestion({
        kind: SuggestionKind.Overwrite,
        nodeName: "node",
      });

      const rename = createSuggestion({
        kind: SuggestionKind.Rename,
        nodeName: "node",
      });
      const data = createSuggestions({ taskId: "task", suggestions: [overwrite, rename] });

      expect(data.taskId).toEqual("task");
      expect(data.suggestions).toHaveLength(2);
    });
  });
});
