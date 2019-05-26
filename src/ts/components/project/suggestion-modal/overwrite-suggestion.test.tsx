import * as React from "react";
import { mount } from "enzyme";
import renderer from "react-test-renderer";
import { Component } from "./overwrite-suggestion";
import { createSuggestion, SuggestionKind } from "../../../domains/task-suggestion";
import { createOverwritePayload, ReplyPayload } from "../../../domains/task-reply";

describe("Project", () => {
  describe("Overwrite suggestion", () => {
    it("render with overwrite suggestion", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Overwrite,
        nodeName: "foo",
      });
      const handler = () => {};
      const tree = renderer.create(<Component selected={false} suggestion={suggestion} onReply={handler} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Overwrite,
        nodeName: "foo",
      });
      const handler = () => {};
      const tree = renderer.create(<Component selected={true} suggestion={suggestion} onReply={handler} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("do not render with other suggestion", () => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Rename,
        nodeName: "foo",
      });
      const handler = () => {};
      const tree = renderer.create(<Component selected={false} suggestion={suggestion} onReply={handler} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("call handler when the component is selected and enter key pressed ", done => {
      const suggestion = createSuggestion({
        kind: SuggestionKind.Overwrite,
        nodeName: "foo",
      });
      const handler = (reply: ReplyPayload) => {
        expect(reply).toEqual(createOverwritePayload());
        done();
      };
      const wrapper = mount(<Component selected={true} suggestion={suggestion} onReply={handler} />);
      wrapper.simulate("keydown", { key: "Enter" });
    });
  });
});
