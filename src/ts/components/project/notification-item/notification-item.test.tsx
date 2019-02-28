import { shallow } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";

import * as Notification from "../../../domains/notification";

import T from "./notification-item";

describe("Project", () => {
  describe("Notification Item", () => {
    it("should print correctly", () => {
      const notification = Notification.createOneShot("id", Notification.Level.Info, "message");
      const onTimeouted = (_: string) => {
        return;
      };

      const tree = renderer.create(<T item={notification} onItemTimeouted={onTimeouted} timeouted={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should call timeout callback if timeouted and transition ended", done => {
      const notification = Notification.createOneShot("id", Notification.Level.Info, "message");
      const onTimeouted = (id: string) => {
        expect(id).toEqual("id");
        done();
      };

      const wrapper = shallow(<T item={notification} onItemTimeouted={onTimeouted} timeouted={false} />);
      wrapper.simulate("exited");
    });

    it("should be able to render warning message", () => {
      const notification = Notification.createOneShot("id", Notification.Level.Warning, "message");
      const onTimeouted = (_: string) => {
        return;
      };

      const tree = renderer.create(<T item={notification} onItemTimeouted={onTimeouted} timeouted={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should ignore progress type notification", () => {
      const notification = Notification.createProgress("id", Notification.Level.Info, {
        process: "process",
        current: 0,
        target: 100,
      });
      const onTimeouted = (_: string) => {
        return;
      };

      const tree = renderer.create(<T item={notification} onItemTimeouted={onTimeouted} timeouted={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
