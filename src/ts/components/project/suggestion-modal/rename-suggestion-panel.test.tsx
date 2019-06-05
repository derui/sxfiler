import * as React from "react";
import { mount } from "enzyme";
import renderer from "react-test-renderer";
import { Component } from "./rename-suggestion-panel";
import { createRenamePayload, ReplyPayload } from "../../../domains/task-reply";

describe("Project", () => {
  describe("Rename suggestion", () => {
    it("render with rename suggestion", () => {
      const handler = () => {};
      const tree = renderer.create(<Component selected={false} onUpdated={handler} nodeName={"node"} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const handler = () => {};
      const tree = renderer.create(<Component selected={true} onUpdated={handler} nodeName={"node"} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("do not render with other suggestion", () => {
      const handler = () => {};
      const tree = renderer.create(<Component selected={false} onUpdated={handler} nodeName={"node"} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("call handler when the new name in the component is changed", done => {
      const handler = (reply: ReplyPayload) => {
        expect(reply).toEqual(createRenamePayload("foo"));
        done();
      };
      const wrapper = mount(<Component selected={true} onUpdated={handler} nodeName="bar" />);
      wrapper.find("input").simulate("change", {
        target: {
          nodeValue: "foo",
        },
      });
    });
  });
});
