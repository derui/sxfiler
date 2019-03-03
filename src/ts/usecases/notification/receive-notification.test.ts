import UseCase from "./receive-notification";
import { Dispatcher } from "../../dispatcher";
import { actions } from "../../actions/notification";
import { createMessage, Level } from "../../domains/notification";

describe("UseCases", () => {
  describe('Notification', () => {
    describe('Receive Notification', () => {
      it('dispatchs action', () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const notification = createMessage("id", Level.Warning, "message");

        new UseCase().execute(dispatcher, { notification });

        expect(fn.mock.calls[0]).toEqual([actions.receiveNotification(notification)]);
      });
    });
  });
});
