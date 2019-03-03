import { shallow } from "enzyme";
import * as React from "react";
import renderer from "react-test-renderer";

import { Actions } from "../../../actions/notification";
import { createMessage, createProgress, Level } from "../../../domains/notification";
import Notifications from "../../../domains/notifications";
import { UseCaseLike } from "../../../usecases/type";
import * as NotificationList from "../../project/notification-list/notification-list";
import { Component } from "./notification-container";

const context = (done?: (arg: any) => void) => ({
  execute<P>(usecase: UseCaseLike<Actions, P>, arg: P) {
    if (done) {
      done(arg);
    }
  },
});

describe("Container", () => {
  describe("Notification Container", () => {
    it("should render correctly", () => {
      const state = {
        notifications: new Notifications([
          createMessage("message", Level.Info, "message"),
          createProgress("progress", Level.Info, {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: new Notifications(),
      };
      const tree = renderer.create(<Component context={context()} state={state} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should execute usecase with hidden notification id", () => {
      const state = {
        notifications: new Notifications([
          createMessage("message", Level.Info, "message"),
          createProgress("progress", Level.Info, {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: new Notifications(),
      };
      const stub = (arg: any) => {
        expect(arg).toEqual({ notificationId: "id" });
      };
      const wrapper = shallow(<Component context={context(stub)} state={state} />);
      wrapper.find(NotificationList.Component).simulate("notificationHidden", "id");
    });
  });
});
