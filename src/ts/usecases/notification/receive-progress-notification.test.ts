import * as actions from "@/actions/notification";
import { Dispatcher } from "@/dispatcher";
import { createProgress } from "@/domains/progress-notification";
import { createUseCase } from "./receive-progress-notification";

describe("UseCases", () => {
  describe("Notification", () => {
    describe("Receive Progress Notification", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const notification = createProgress("id", {
          current: 15,
          process: "process",
          targeted: 30,
        });

        createUseCase().execute(dispatcher, { notification });

        expect(fn.mock.calls[0]).toEqual([actions.receiveProgress(notification)]);
      });

      it("dispatch action to remove when current is greater equal target", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const notification = createProgress("id", {
          current: 30,
          process: "process",
          targeted: 30,
        });

        createUseCase().execute(dispatcher, { notification });

        expect(fn).toHaveBeenCalledWith(actions.remove(notification.id));
      });
    });
  });
});
