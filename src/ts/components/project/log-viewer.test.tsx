import { h } from "preact";
import { render } from "preact-render-to-string";

import { Component as T } from "./log-viewer";

describe("Project", () => {
  describe("Log Viewer", () => {
    it("should render empty", () => {
      const tree = render(<T entries={[]} hidden={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should render items each level", () => {
      const entries = ["info", "error", "warning"];
      const tree = render(<T entries={entries} hidden={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should render empty when hidden it", () => {
      const entries = ["info", "error", "warning"];
      const tree = render(<T entries={entries} hidden={true} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
