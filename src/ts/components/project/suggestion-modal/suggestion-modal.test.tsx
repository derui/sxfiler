import * as React from "react";
import { mount } from "enzyme";
import * as Modal from "./suggestion-modal";
import { createSuggestion, SuggestionKind } from "../../../domains/task-suggestion";
import { Component as OverwriteSuggestionPanel } from "./overwrite-suggestion-panel";
import { Component as RenameSuggestionPanel } from "./rename-suggestion-panel";

describe("Project", () => {
  describe("Suggestion Modal", () => {
    it("do not render when no any suggestion", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedSuggestion: 0,
            suggestions: [],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(0);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(0);
    });

    it("render with overwrite suggestion", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedSuggestion: 0,
            suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite, nodeName: "foo" })],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(1);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(0);
    });
    it("render with rename suggestion", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedSuggestion: 0,
            suggestions: [createSuggestion({ kind: SuggestionKind.Rename, nodeName: "foo" })],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(0);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(1);
    });
  });
});
