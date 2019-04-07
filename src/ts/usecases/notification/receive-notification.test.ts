import { actions } from "../../actions/notification";
import { Dispatcher } from "../../dispatcher";
import { createMessage, Level } from "../../domains/notification";
import { createUseCase } from "./receive-notification";

describe("UseCases", () => {
  describe("Notification", () => {
    describe("Receive Notification", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const notification = createMessage("id", Level.Warning, "message");

        createUseCase().execute(dispatcher, { notification });

        expect(fn.mock.calls[0]).toEqual([actions.receiveNotification(notification)]);
      });
    });
  });
});
