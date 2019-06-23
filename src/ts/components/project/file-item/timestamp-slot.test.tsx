import * as React from "react";
import renderer from "react-test-renderer";
import T from "./timestamp-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Timestamp slot", () => {
      it("should show formatted timestamp", () => {
        const date = new Date("2019-02-28T01:02:03Z");
        const tree = renderer.create(<T timestamp={date} />).toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
