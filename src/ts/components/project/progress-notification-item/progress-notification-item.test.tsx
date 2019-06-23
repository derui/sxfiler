import * as React from "react";
import renderer from "react-test-renderer";

import * as Notification from "../../../domains/progress-notification";

import { Component as T } from "./progress-notification-item";

describe("Project", () => {
  describe("Progress Notification Item", () => {
    it("should print correctly", () => {
      const notification = Notification.createProgress("id", {
        process: "target process",
        current: 10,
        target: 100,
      });
      const tree = renderer.create(<T body={notification.body} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should hide when current value is equvalent to target value", () => {
      const notification = Notification.createProgress("id", {
        process: "target process",
        current: 101,
        target: 100,
      });
      const tree = renderer.create(<T body={notification.body} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should not over 100% of width", () => {
      const notification = Notification.createProgress("id", {
        process: "target process",
        current: 101,
        target: 100,
      });
      const tree = renderer.create(<T body={notification.body} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
