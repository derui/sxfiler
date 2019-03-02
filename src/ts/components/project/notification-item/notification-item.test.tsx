import { shallow } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";

import * as Notification from "../../../domains/notification";

import { Component as T } from "./notification-item";

describe("Project", () => {
  describe("Notification Item", () => {
    it("should print correctly", () => {
      const notification = Notification.createMessage("id", Notification.Level.Info, "message");
      const onExited = () => {
        return;
      };

      const tree = renderer
        .create(
          <T body={notification.getMessageBody()} level={notification.level} onExited={onExited} timeouted={false} />
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should call timeout callback if timeouted and transition ended", done => {
      const notification = Notification.createMessage("id", Notification.Level.Info, "message");
      const onExited = () => {
        done();
      };

      const wrapper = shallow(
        <T body={notification.getMessageBody()} level={notification.level} onExited={onExited} timeouted={false} />
      );
      wrapper.simulate("exited");
    });

    it("should be able to render warning message", () => {
      const notification = Notification.createMessage("id", Notification.Level.Warning, "message");
      const onExited = () => {
        return;
      };

      const tree = renderer
        .create(
          <T body={notification.getMessageBody()} level={notification.level} onExited={onExited} timeouted={false} />
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
