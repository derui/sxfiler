import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";

import * as N from "@/domains/progress-notification";
import { Component as T } from "./progress-notification-list";

describe("Project", () => {
  describe("Progress Notification List", () => {
    it("can render correctly", () => {
      const notifications = [
        N.createProgress("id", {
          process: "process",
          current: 10,
          target: 100,
        }),
      ];
      const tree = renderer.create(wrap(<T notifications={notifications} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can render with empty list of notification", () => {
      const tree = renderer.create(wrap(<T notifications={[]} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should render progresses", () => {
      const notifications = [
        N.createProgress("id", {
          process: "foo",
          current: 9,
          target: 10,
        }),
        N.createProgress("id2", {
          process: "bar",
          current: 10,
          target: 11,
        }),
      ];
      const tree = renderer.create(wrap(<T notifications={notifications} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
