import { actions } from "@/actions/notification";
import { Dispatcher } from "@/dispatcher";
import { createMessage, Level } from "@/domains/message-notification";
import { createUseCase } from "./receive-message-notification";

describe("UseCases", () => {
  describe("Notification", () => {
    describe("Receive Message Notification", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const notification = createMessage({ id: "id", level: Level.Warning, message: "message" });

        createUseCase().execute(dispatcher, { notification });

        expect(fn.mock.calls[0]).toEqual([actions.receiveMessage(notification)]);
      });
    });
  });
});
