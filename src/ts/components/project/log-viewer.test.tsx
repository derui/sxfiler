import { h } from "preact";
import { render } from "preact-render-to-string";

import { Component as T } from "./log-viewer";
import { LogEventCreators } from "@/modules/log-event";

describe("Project", () => {
  describe("Log Viewer", () => {
    it("should render empty", () => {
      const tree = render(<T entries={[]} hidden={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should render items each level", () => {
      const entries = [
        LogEventCreators.createKeymapReload(new Date(0)),
        LogEventCreators.createDeleteItem(new Date(1), "full path"),
      ];
      const tree = render(<T entries={entries} hidden={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should render empty when hidden it", () => {
      const tree = render(<T entries={[]} hidden={true} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
