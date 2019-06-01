import * as React from "react";
import { mount } from "enzyme";
import renderer from "react-test-renderer";
import { Component } from "./rename-suggestion-panel";
import { createSuggestion, SuggestionKind } from "../../../domains/task-suggestion";
import { createRenamePayload, ReplyPayload } from "../../../domains/task-reply";

describe("Project", () => {
  describe("Rename suggestion", () => {
    it("render with rename suggestion", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const handler = () => {};
      const tree = renderer
        .create(<Component selected={false} suggestion={suggestion} onReply={handler} nodeName={"node"} />)
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const handler = () => {};
      const tree = renderer
        .create(<Component selected={true} suggestion={suggestion} onReply={handler} nodeName={"node"} />)
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("do not render with other suggestion", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const handler = () => {};
      const tree = renderer
        .create(<Component selected={false} suggestion={suggestion} onReply={handler} nodeName={"node"} />)
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("call handler when the component is selected and enter key pressed ", done => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const handler = (reply: ReplyPayload) => {
        expect(reply).toEqual(createRenamePayload("foo"));
        done();
      };
      const wrapper = mount(<Component selected={true} suggestion={suggestion} onReply={handler} nodeName={"foo"} />);
      wrapper.simulate("keydown", { key: "Enter" });
    });

    it("change name when changed value", done => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
      });
      const handler = (reply: ReplyPayload) => {
        expect(reply).toEqual(createRenamePayload("next"));
        done();
      };
      const wrapper = mount(<Component selected={true} suggestion={suggestion} onReply={handler} nodeName={"node"} />);
      wrapper.find("input").simulate("change", { target: { nodeValue: "next" } });
      wrapper.simulate("keydown", { key: "Enter" });
    });
  });
});
