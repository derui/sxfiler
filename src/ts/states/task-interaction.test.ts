import * as S from "./task-interaction";
import { createSuggestions, createSuggestion, SuggestionKind } from "../domains/task-suggestion";

describe("States", () => {
  describe("Task Interaction", () => {
    it("create new empty state", () => {
      const state = S.empty();

      expect(state.currentTaskId).toBeUndefined;
      expect(state.suggestions).toBeUndefined;
      expect(state.currentSuggestionIndex).toBeUndefined;
      expect(state.operating).toBeFalsy;
    });

    it("give suggestions when current state is empty", () => {
      const suggestions = createSuggestions({
        taskId: "task",
        suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite, nodeName: "node" })],
      });
      const state = S.gaveSuggestions(S.empty(), suggestions);

      expect(state.currentTaskId).toEqual("task");
      expect(state.suggestions).toEqual(suggestions);
      expect(state.currentSuggestionIndex).toEqual(0);
      expect(state.operating).toBeTruthy;
    });

    it("queue interaction when operating other interaction", () => {
      const suggestions_1 = createSuggestions({
        taskId: "task1",
        suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite, nodeName: "node1" })],
      });
      const suggestions_2 = createSuggestions({
        taskId: "task2",
        suggestions: [createSuggestion({ kind: SuggestionKind.Rename, nodeName: "node2" })],
      });
      let state = S.gaveSuggestions(S.empty(), suggestions_1);
      state = S.gaveSuggestions(state, suggestions_2);

      expect(state.suggestionQueue).toHaveLength(1);
    });

    it("change suggestion index that is current selected", () => {
      const suggestions = createSuggestions({
        taskId: "task1",
        suggestions: [
          createSuggestion({ kind: SuggestionKind.Overwrite, nodeName: "node1" }),
          createSuggestion({ kind: SuggestionKind.Rename, nodeName: "node1" }),
        ],
      });
      const state = S.selectSuggestion(S.gaveSuggestions(S.empty(), suggestions), 1);

      expect(state.currentSuggestionIndex).toEqual(1);
    });

    it("do not allow to select index outer available suggestions", () => {
      const suggestions = createSuggestions({
        taskId: "task1",
        suggestions: [
          createSuggestion({ kind: SuggestionKind.Overwrite, nodeName: "node1" }),
          createSuggestion({ kind: SuggestionKind.Rename, nodeName: "node1" }),
        ],
      });
      const state = S.selectSuggestion(S.gaveSuggestions(S.empty(), suggestions), -1);

      expect(state.currentSuggestionIndex).toEqual(0);
    });
  });
});
