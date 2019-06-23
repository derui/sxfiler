import { shallow } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";

import { Side, State } from "../../../../states/file-list";
import { Component } from "./file-list-container";
import { createFiler } from "../../../../domains/filer";

describe("Container", () => {
  describe("File List Container", () => {
    it("should render correctly", () => {
      const state: State = {
        left: createFiler({
          id: "left",
          name: "right",
          location: "/left",
          items: [],
          currentCursorIndex: 0,
        }),
        right: createFiler({
          id: "right",
          name: "right",
          location: "/right",
          items: [],
          currentCursorIndex: 0,
        }),
        currentSide: Side.Left,
      };
      const tree = renderer.create(<Component state={state} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should mark current side of filer", () => {
      const state: State = {
        left: createFiler({
          id: "left",
          name: "left",
          location: "/left",
          items: [],
          currentCursorIndex: 0,
        }),
        right: createFiler({
          id: "right",
          name: "right",
          location: "/right",
          items: [],
          currentCursorIndex: 0,
        }),
        currentSide: Side.Right,
      };
      const wrapper = shallow(<Component state={state} />);

      const focused = wrapper.findWhere(child => child.key() === Side.Right).prop("focused");
      expect(focused).toStrictEqual(true);
    });
  });
});
