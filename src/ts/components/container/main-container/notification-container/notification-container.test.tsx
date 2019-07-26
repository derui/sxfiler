import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";

import { Actions } from "@/actions/notification";
import { createNotifications } from "@/domains/progress-notifications";
import { UseCaseLike } from "@/usecases/type";
import { Component } from "./notification-container";
import { LocatorContext } from "@/locator";
import { createProgress } from "@/domains/progress-notification";

const context = (done?: (arg: any) => void) => ({
  use<P>(_: UseCaseLike<Actions, P>) {
    return {
      execute(args: P) {
        if (done) {
          done(args);
        }
      },
    };
  },
});

describe("Container", () => {
  describe("Notification Container", () => {
    it("should render correctly when context not initialized yet", () => {
      const locator = {};
      const state = {
        progresses: createNotifications([
          createProgress("progress", {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: createNotifications([]),
      };
      const tree = renderer
        .create(
          wrap(
            <LocatorContext.Provider value={locator}>
              <Component state={state} />
            </LocatorContext.Provider>
          )
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should render correctly when context initialized", () => {
      const locator = { context: context(jest.fn()) };
      const state = {
        progresses: createNotifications([
          createProgress("progress", {
            process: "process",
            target: 100,
            current: 10,
          }),
        ]),
        timeouts: createNotifications([]),
      };
      const tree = renderer
        .create(
          wrap(
            <LocatorContext.Provider value={locator}>
              <Component state={state} />
            </LocatorContext.Provider>
          )
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
