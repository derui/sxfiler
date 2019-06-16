import * as S from "./task-interaction";
import { createSuggestions, createSuggestion, SuggestionKind } from "../domains/task-suggestion";
import { createRenamePayload, createOverwritePayload } from "../domains/task-reply";

describe("States", () => {
  describe("Task Interaction", () => {
    it("create new empty state", () => {
      const state = S.empty();

      expect(state.currentTaskId).toBeUndefined;
      expect(state.replies).toBeUndefined;
      expect(state.currentReplyIndex).toBeUndefined;
      expect(state.operating).toBeFalsy;
    });

    it("give suggestions when current state is empty", () => {
      const suggestions = createSuggestions({
        taskId: "task",
        nodeName: "node",
        suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite })],
      });
      const state = S.giveSuggestions(S.empty(), suggestions);

      expect(state.currentTaskId).toEqual("task");
      expect(state.replies).toEqual([createOverwritePayload()]);
      expect(state.currentReplyIndex).toEqual(0);
      expect(state.operating).toBeTruthy;
    });

    it("queue interaction when operating other interaction", () => {
      const suggestions_1 = createSuggestions({
        taskId: "task1",
        nodeName: "node",
        suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite })],
      });
      const suggestions_2 = createSuggestions({
        taskId: "task2",
        nodeName: "node",
        suggestions: [createSuggestion({ kind: SuggestionKind.Rename })],
      });
      let state = S.giveSuggestions(S.empty(), suggestions_1);
      state = S.giveSuggestions(state, suggestions_2);

      expect(state.replyQueue).toHaveLength(1);
    });

    it("change suggestion index that is current selected", () => {
      const suggestions = createSuggestions({
        taskId: "task1",
        nodeName: "node",
        suggestions: [
          createSuggestion({ kind: SuggestionKind.Overwrite }),
          createSuggestion({ kind: SuggestionKind.Rename }),
        ],
      });
      const state = S.selectReply(S.giveSuggestions(S.empty(), suggestions), 1);

      expect(state.currentReplyIndex).toEqual(1);
    });

    it("do not allow to select index outer available suggestions", () => {
      const suggestions = createSuggestions({
        taskId: "task1",
        nodeName: "node",
        suggestions: [
          createSuggestion({ kind: SuggestionKind.Overwrite }),
          createSuggestion({ kind: SuggestionKind.Rename }),
        ],
      });
      const state = S.selectReply(S.giveSuggestions(S.empty(), suggestions), -1);

      expect(state.currentReplyIndex).toEqual(0);
    });

    it("update payload in state", () => {
      const suggestions = createSuggestions({
        taskId: "task1",
        nodeName: "node",
        suggestions: [
          createSuggestion({ kind: SuggestionKind.Overwrite }),
          createSuggestion({ kind: SuggestionKind.Rename }),
        ],
      });
      const payload = createRenamePayload("new_node");
      let state = S.selectReply(S.giveSuggestions(S.empty(), suggestions), 1);
      state = S.updateCurrentReply(state, payload);

      expect(state.currentReply()).toEqual(payload);
    });
  });
});