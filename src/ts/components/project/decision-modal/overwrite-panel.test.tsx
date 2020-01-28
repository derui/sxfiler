import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component } from "./overwrite-panel";

describe("Project", () => {
  describe("Overwrite decision", () => {
    it("render with overwrite decision", () => {
      const tree = render(<Component selected={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const tree = render(<Component selected={true} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
