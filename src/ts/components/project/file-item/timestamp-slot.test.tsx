import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component as T } from "./timestamp-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Timestamp slot", () => {
      it("should show formatted timestamp", () => {
        const date = new Date("2019-02-28T01:02:03Z");
        const tree = render(<T timestamp={date} />);

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
