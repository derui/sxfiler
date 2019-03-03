import * as React from "react";
import renderer from "react-test-renderer";

import * as N from "../../../domains/notification";
import { Component as T } from "./notification-list";

describe("Project", () => {
  describe("Notification List", () => {
    it("can render correctly", () => {
      const handle = () => { return };
      const notifications = [N.createMessage("id", N.Level.Info, "message")];
      const tree = renderer
        .create(<T notifications={notifications} timeouts={[]} onNotificationHidden={handle} />)
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can render with empty list of notification", () => {
      const handle = () => { return };
      const tree = renderer.create(<T notifications={[]} timeouts={[]} onNotificationHidden={handle} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should set timeout specified", () => {
      const handle = () => { return };
      const notifications = [
        N.createMessage("id", N.Level.Info, "message"),
        N.createMessage("id2", N.Level.Warning, "message"),
      ];
      const tree = renderer
        .create(<T notifications={notifications} timeouts={["id"]} onNotificationHidden={handle} />)
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
