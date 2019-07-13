import * as React from "react";
import renderer from "react-test-renderer";

import { Component } from "./log-viewer-container";
import { empty, pushEntry } from "@/states/log-entry";
import { Level, createMessage } from "@/domains/message-notification";

describe("Container", () => {
  describe("Log Viewer Container", () => {
    it("should render correctly", () => {
      const state = pushEntry(empty(), createMessage({ id: "id", level: Level.Info, message: "message" }));
      const tree = renderer.create(<Component state={state} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
