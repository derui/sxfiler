import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component } from "./log-viewer-container";
import { emptyState } from "@/modules";

describe("Container", () => {
  describe("Log Viewer Container", () => {
    it("should render correctly", () => {
      const tree = render(<Component state={emptyState} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
