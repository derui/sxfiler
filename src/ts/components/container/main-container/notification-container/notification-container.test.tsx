import * as React from "react";
import renderer from "react-test-renderer";

import { Actions } from "../../../../actions/notification";
import { createMessage, createProgress, Level } from "../../../../domains/notification";
import { createNotifications } from "../../../../domains/notifications";
import { UseCaseLike } from "../../../../usecases/type";
import { Component } from "./notification-container";
import LocatorContext from "../../../../locator";

const context = (done?: (arg: any) => void) => ({
  execute<P>(_: UseCaseLike<Actions, P>, arg: P) {
    if (done) {
      done(arg);
    }
  },
});

describe("Container", () => {
  describe("Notification Container", () => {
    it("should render correctly when context not initialized yet", () => {
      const locator = {};
      const state = {
        notifications: createNotifications([
          createMessage("message", Level.Info, "message"),
          createProgress("progress", Level.Info, {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: createNotifications([]),
      };
      const tree = renderer
        .create(
          <LocatorContext.Provider value={locator}>
            <Component state={state} />
          </LocatorContext.Provider>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should render correctly when context initialized", () => {
      const locator = { context: context(jest.fn()) };
      const state = {
        notifications: createNotifications([
          createMessage("message", Level.Info, "message"),
          createProgress("progress", Level.Info, {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: createNotifications([]),
      };
      const tree = renderer
        .create(
          <LocatorContext.Provider value={locator}>
            <Component state={state} />
          </LocatorContext.Provider>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
