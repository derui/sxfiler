import * as React from "react";
import { mount } from "enzyme";
import * as Modal from "./suggestion-modal";
import { createSuggestion, SuggestionKind } from "../../../domains/task-suggestion";
import { Component as OverwriteSuggestion } from "./overwrite-suggestion";
import { Component as RenameSuggestion } from "./rename-suggestion";

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

      expect(tree.find(OverwriteSuggestion)).toHaveLength(0);
      expect(tree.find(RenameSuggestion)).toHaveLength(0);
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

      expect(tree.find(OverwriteSuggestion)).toHaveLength(1);
      expect(tree.find(RenameSuggestion)).toHaveLength(0);
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

      expect(tree.find(OverwriteSuggestion)).toHaveLength(0);
      expect(tree.find(RenameSuggestion)).toHaveLength(1);
    });
  });
});
