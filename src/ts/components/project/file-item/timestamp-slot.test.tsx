import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";
import { Component as T } from "./timestamp-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Timestamp slot", () => {
      it("should show formatted timestamp", () => {
        const date = new Date("2019-02-28T01:02:03Z");
        const tree = renderer.create(wrap(<T timestamp={date} />)).toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
