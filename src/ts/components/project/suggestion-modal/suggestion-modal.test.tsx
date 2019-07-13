import * as React from "react";
import { mount } from "enzyme";
import * as Modal from "./suggestion-modal";
import { Component as OverwriteSuggestionPanel } from "./overwrite-suggestion-panel";
import { Component as RenameSuggestionPanel } from "./rename-suggestion-panel";
import { createRenamePayload, createOverwritePayload } from "@/domains/task-reply";

describe("Project", () => {
  describe("Suggestion Modal", () => {
    it("do not render when no any suggestion", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedReply: 0,
            replies: [],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(0);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(0);
    });

    it("render with reply to overwrite", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedReply: 0,
            replies: [createOverwritePayload()],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(1);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(0);
    });
    it("render with reply to rename", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedReply: 0,
            replies: [createRenamePayload("foo")],
            onReply: () => {},
          }}
        />
      );

      expect(tree.find(OverwriteSuggestionPanel)).toHaveLength(0);
      expect(tree.find(RenameSuggestionPanel)).toHaveLength(1);
    });
  });
});
