import { shallow } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";

import FilerFactory from "../../../../domains/filer-factory";
import { Side, State } from "../../../../types/store-state/file-list";
import { Component } from "./file-list-container";

describe("Container", () => {
  describe("File List Container", () => {
    it("should render correctly", () => {
      const state: State = {
        initialized: true,
        left: FilerFactory.create({
          id: "left",
          location: "/left",
          nodes: [],
        }),
        right: FilerFactory.create({
          id: "right",
          location: "/right",
          nodes: [],
        }),
        currentSide: Side.Left,
      };
      const tree = renderer.create(<Component state={state} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should mark current side of filer", () => {
      const state: State = {
        initialized: true,
        left: FilerFactory.create({
          id: "left",
          location: "/left",
          nodes: [],
        }),
        right: FilerFactory.create({
          id: "right",
          location: "/right",
          nodes: [],
        }),
        currentSide: Side.Right,
      };
      const wrapper = shallow(<Component state={state} />);

      const focused = wrapper.findWhere(child => child.key() === Side.Right).prop("focused");
      expect(focused).toStrictEqual(true);
    });
  });
});
