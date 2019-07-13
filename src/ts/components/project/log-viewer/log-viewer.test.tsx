import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";

import { Component as T } from "./log-viewer";
import { createMessage, Level } from "@/domains/message-notification";

describe("Project", () => {
  describe("Log Viewer", () => {
    it("should render empty", () => {
      const tree = renderer.create(wrap(<T entries={[]} hidden={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should render items each level", () => {
      const entries = [
        createMessage({ id: "id1", message: "info", level: Level.Info }),
        createMessage({ id: "id2", message: "error", level: Level.Error }),
        createMessage({ id: "id3", message: "warning", level: Level.Warning }),
      ];
      const tree = renderer.create(wrap(<T entries={entries} hidden={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should render empty when hidden it", () => {
      const entries = [
        createMessage({ id: "id1", message: "info", level: Level.Info }),
        createMessage({ id: "id2", message: "error", level: Level.Error }),
        createMessage({ id: "id3", message: "warning", level: Level.Error }),
      ];
      const tree = renderer.create(wrap(<T entries={entries} hidden={true} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
