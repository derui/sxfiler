import * as React from "react";
import renderer from "react-test-renderer";

import * as N from "../../../domains/notification";
import T from "./notification-list";

describe("Project", () => {
  describe("Notification List", () => {
    it("can render correctly", () => {
      const notifications = [
        N.createOneShot("id", N.Level.Info, "message"),
      ];
      const tree = renderer.create(<T notifications={notifications} timeouts={[]} onItemTimeouted={() => { }} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can render with empty list of notification", () => {
      const tree = renderer.create(<T notifications={[]} timeouts={[]} onItemTimeouted={() => { }} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should set timeout specified", () => {
      const notifications = [
        N.createOneShot("id", N.Level.Info, "message"),
        N.createOneShot("id2", N.Level.Warning, "message"),
      ];
      const tree = renderer.create(<T notifications={notifications} timeouts={["id"]} onItemTimeouted={() => { }} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
