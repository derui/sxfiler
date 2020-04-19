import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component } from "./log-viewer-container";

describe("Container", () => {
  describe("Log Viewer Container", () => {
    it("should render correctly", () => {
      const tree = render(<Component />);

      expect(tree).toMatchSnapshot();
    });
  });
});
