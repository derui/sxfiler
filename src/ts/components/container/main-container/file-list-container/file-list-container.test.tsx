import { mount } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";

import { Side, State } from "@/states/file-list";
import { Component } from "./file-list-container";
import { createFiler } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";

describe("Container", () => {
  describe("File List Container", () => {
    const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

    it("should render correctly", () => {
      const state: State = {
        left: createFiler({
          id: "left",
          name: "right",
          location: "/left",
          items: [],
          currentCursorIndex: 0,
          history,
        }),
        right: createFiler({
          id: "right",
          name: "right",
          location: "/right",
          items: [],
          currentCursorIndex: 0,
          history,
        }),
        currentSide: Side.Left,
      };
      const tree = renderer.create(wrap(<Component state={state} />)).toJSON();

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
          history,
        }),
        right: createFiler({
          id: "right",
          name: "right",
          location: "/right",
          items: [],
          currentCursorIndex: 0,
          history,
        }),
        currentSide: Side.Right,
      };
      const wrapper = mount(wrap(<Component state={state} />));

      const focused = wrapper.findWhere(child => child.key() === Side.Right).prop("focused");
      expect(focused).toStrictEqual(true);
    });
  });
});
